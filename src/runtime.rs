use serde::{Deserialize, Serialize};
use std::sync::{Arc, RwLock};
use tokio::task::JoinHandle;
use uuid::Uuid;
use wasi_common::pipe::WritePipe;
use wasmtime::{Config, Engine, Linker, Module, Store};
use wasmtime_wasi::sync::WasiCtxBuilder;

pub struct Runtime {
    instances: Vec<Instance>
}

struct Instance {
    id: Uuid,
    package: String,
    engine: Arc<Engine>,
    task: JoinHandle<()>,
    output: Arc<RwLock<Option<String>>>
}

#[derive(Debug, Deserialize, Serialize)]
pub struct InstanceInfo {
    pub id: String,
    pub package: String,
    pub state: InstanceState
}

#[derive(Debug, Deserialize, Serialize)]
pub enum InstanceState {
    Running,
    Terminated
}

impl Runtime {
    pub fn new() -> Self {
        Self {
            instances: Vec::new()
        }
    }

    pub fn ps(&self) -> Vec<InstanceInfo> {
        self.instances
            .iter()
            .map(InstanceInfo::from_instance)
            .collect()
    }

    pub fn res(&mut self, id: &str) -> Option<String> {
        let instance = self.instances
            .iter()
            .find(|i| i.id.to_string() == id);

        instance.and_then(|i| {
            let output = i.output.read().unwrap();
            output.clone()
        })
    }

    pub fn launch(&mut self, package: String, args: Vec<String>, bytecode: Vec<u8>) -> anyhow::Result<Uuid> {
        let id = Uuid::new_v4();

        let engine = Engine::new(Config::new().epoch_interruption(true))?;
        let engine = Arc::new(engine);
        let output = Arc::new(RwLock::new(None));

        let task_engine = engine.clone();
        let task_output = output.clone();
        let task = tokio::spawn(async move {
            let result = Self::run(task_engine, args, bytecode).await;

            let output_str = match result {
                Err(e) => format!("{:?}", e),
                Ok(out) => out
            };

            let mut output = task_output.write().unwrap();
            *output = Some(output_str);
        });

        let instance = Instance {
            id,
            package,
            engine,
            task,
            output
        };

        self.instances.push(instance);

        Ok(id)
    }

    pub fn kill(&self, id: &str) {
        let instance = self.instances
            .iter()
            .find(|i| i.id.to_string() == id);

        if let Some(instance) = instance {
            instance.engine.increment_epoch();
        }
    }

    pub fn check_wasm(bytecode: Vec<u8>) -> anyhow::Result<()> {
        let engine = Engine::new(&Config::new())?;
        let _ = Module::new(&engine, bytecode)?;

        Ok(())
    }

    async fn run(engine: Arc<Engine>, args: Vec<String>, bytecode: Vec<u8>) -> anyhow::Result<String> {
        let args = args;
        let mut linker = Linker::new(&engine);

        wasmtime_wasi::add_to_linker(&mut linker, |s| s)?;

        let stdout = WritePipe::new_in_memory();

        {
            let wasi = WasiCtxBuilder::new()
                .args(&args)?
                .stdout(Box::new(stdout.clone()))
                .build();

            let mut store = Store::new(&engine, wasi);
            store.set_epoch_deadline(1);

            let module = Module::new(&engine, bytecode)?;
            linker.module(&mut store, "", &module)?;

            linker
                .get_default(&mut store, "")?
                .typed::<(), ()>(&store)?
                .call(&mut store, ())?;
        }

        let stdout_bytes : Vec<u8> = stdout.try_into_inner().unwrap().into_inner();
        let stdout_str = std::str::from_utf8(&stdout_bytes).map(String::from)?;
        Ok(stdout_str)
    }
}

impl InstanceInfo {
    fn from_instance(instance: &Instance) -> Self {
        let state = if instance.task.is_finished() {
            InstanceState::Terminated
        } else {
            InstanceState::Running
        };

        Self {
            id: instance.id.to_string(),
            package: instance.package.clone(),
            state
        }
    }
}
