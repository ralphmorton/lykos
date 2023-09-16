use std::path::Path;

#[derive(Debug)]
pub struct Registry {
    root: String,
    packages: Vec<Package>
}

#[derive(Debug)]
pub struct Package {
    file_name: String
}

impl Registry {
    pub fn new(root: &str) -> anyhow::Result<Self> {
        let root = root.to_string();

        let packages = std::fs::read_dir(&root)?
            .filter_map(|file| {
                let file = file.ok()?;
                let file_type = file.file_type().ok()?;

                if file_type.is_file() {
                    let file_name = file.file_name().into_string().ok()?;
                    Some(Package {
                        file_name
                    })
                } else {
                    None
                }
            })
            .collect();

        Ok(Self {
            root,
            packages
        })
    }

    pub fn ls(&self) -> impl Iterator<Item = &Package> {
        self.packages.iter()
    }

    pub fn rm(&mut self, file_name: &str) -> anyhow::Result<()> {
        let path = Path::new(&self.root).join(file_name);

        std::fs::remove_file(path)?;

        if let Some(package_ix) = self.packages.iter().position(|pkg| pkg.file_name == file_name) {
            self.packages.remove(package_ix);
        }

        Ok(())
    }

    pub fn install(&mut self, file_name: &str, bytes: impl AsRef<[u8]>) -> anyhow::Result<()> {
        let path = Path::new(&self.root).join(file_name);

        std::fs::write(path, bytes)?;

        self.packages.push(Package {
            file_name: file_name.to_string()
        });

        Ok(())
    }

    pub fn read(&self, file_name: &str) -> anyhow::Result<Vec<u8>> {
        let path = Path::new(&self.root).join(file_name);
        let bytes = std::fs::read(path)?;
        Ok(bytes)
    }
}

impl Package {
    pub fn file_name(&self) -> &str {
        &self.file_name
    }
}
