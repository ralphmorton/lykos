use serde::{Deserialize, Serialize};
use std::sync::Arc;
use tokio::task::JoinHandle;
use uuid::Uuid;
use wasmer::{Module, Store};
use wasmer_wasix::{LocalNetworking, WasiEnv};
use wasmer_wasix::runtime::PluggableRuntime;
use wasmer_wasix::runtime::task_manager::tokio::{RuntimeOrHandle, TokioTaskManager};

#[derive(Debug)]
pub struct Runtime {
    instances: Vec<Instance>
}

#[derive(Debug)]
struct Instance {
    id: Uuid,
    package: String,
    task: JoinHandle<anyhow::Result<()>>
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

    pub fn launch(&mut self, package: &str, bytecode: Vec<u8>) -> Uuid {
        let id = Uuid::new_v4();
        let package = package.to_string();
        let task = tokio::spawn(Self::run(package.clone(), bytecode));

        let instance = Instance {
            id,
            package,
            task
        };

        self.instances.push(instance);

        id
    }

    pub fn kill(&self, id: &str) {
        self.instances
            .iter()
            .find(|i| i.id.to_string() == id)
            .map(|i| i.task.abort());
    }

    async fn run(package: String, bytecode: Vec<u8>) -> anyhow::Result<()> {
        let store = Store::default();
        let module = Module::new(&store, &bytecode)?;

        let tokio_handle = RuntimeOrHandle::Handle(tokio::runtime::Handle::current());
        let mut runtime = PluggableRuntime::new(Arc::new(TokioTaskManager::new(tokio_handle)));
        runtime.set_networking_implementation(LocalNetworking::default());

        WasiEnv::builder(&package)
            .runtime(Arc::new(runtime))
            .run_with_store_async(module, store)?;

        Ok(())
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
