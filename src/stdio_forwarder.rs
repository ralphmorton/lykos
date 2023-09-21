use std::io::{Read, Write};

pub struct StdioForwarder {
    data: Vec<u8>
}

impl StdioForwarder {
    pub fn new() -> Self {
        Self {
            data: Vec::new()
        }
    }

    pub fn push(&mut self, data: &[u8]) {
        self.data.extend_from_slice(data);
    }

    pub fn pull(&mut self, max: usize, buf: &mut [u8]) -> usize {
        let max = std::cmp::min(max, self.data.len());

        let res = self.data.split_at(max).0;
        let res_len = res.len();
        buf[0..res.len()].copy_from_slice(res);

        self.data.drain(0..res.len());

        res_len
    }

    pub fn pull_all(&mut self) -> Vec<u8> {
        let res = self.data.to_vec();
        self.data = Vec::new();
        res
    }
}

impl Read for StdioForwarder {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        let n = self.pull(buf.len(), buf);

        Ok(n)
    }
}

impl Write for StdioForwarder {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.push(buf);

        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}
