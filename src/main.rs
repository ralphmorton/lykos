use axum::{Router, Server};
use axum::body::Body;
use axum::extract::{Json, State, Path};
use axum::extract::ws::{Message, WebSocket, WebSocketUpgrade};
use axum::http::Request;
use axum::response::IntoResponse;
use axum::routing::{get, post};
use base64::Engine;
use clap::Parser;
use lykos::error::AppError;
use lykos::registry::{Package, Registry};
use lykos::runtime::{InstanceInfo, Runtime};
use lykos::stdio_forwarder::StdioForwarder;
use serde::{Deserialize, Serialize};
use std::sync::{Arc, RwLock};
use tower::ServiceExt;
use tower_http::services::{ServeDir, ServeFile};
use tower_http::trace::TraceLayer;

#[derive(Parser)]
struct Args {
    #[arg(long = "root")]
    root: String
}

#[derive(Clone)]
struct AppState {
    registry: Arc<RwLock<Registry>>,
    runtime: Arc<RwLock<Runtime>>
}

#[tokio::main]
async fn main() {
    tracing_subscriber::fmt::init();

    let args = Args::parse();

    let registry = Registry::new(&args.root).expect("couldnt instantiate registry");
    let runtime = Runtime::new();
    let state = AppState {
        registry: Arc::new(RwLock::new(registry)),
        runtime: Arc::new(RwLock::new(runtime))
    };

    let spa = ServeDir::new("static")
        .not_found_service(ServeFile::new("static/index.html"));

    let app = Router::new()
        .route("/ls", post(ls))
        .route("/rm", post(rm))
        .route("/install", post(install))
        .route("/ps", post(ps))
        .route("/launch", post(launch))
        .route("/kill", post(kill))
        .route("/res", post(res))
        .route("/attach/:package", get(attach))
        .nest_service(
            "/assets",
            get(move |request: Request<Body>| async {
                spa.oneshot(request).await
            })
        )
        .fallback_service(ServeFile::new("static/index.html"))
        .layer(TraceLayer::new_for_http())
        .with_state(state);

    Server::bind(&"0.0.0.0:3000".parse().unwrap())
        .serve(app.into_make_service())
        .await
        .unwrap();
}

async fn ls(State(state): State<AppState>) -> Json<Vec<String>> {
    let registry = state.registry.read().unwrap();

    let mut packages = registry
        .ls()
        .map(Package::file_name)
        .map(String::from)
        .collect::<Vec<_>>();

    packages.sort();

    Json(packages)
}

#[derive(Deserialize, Serialize)]
struct Rm {
    package: String
}

async fn rm(State(state): State<AppState>, Json(rm): Json<Rm>) -> Result<Json<()>, AppError> {
    let mut registry = state.registry.write().unwrap();

    registry.rm(&rm.package)?;

    Ok(Json(()))
}

#[derive(Deserialize, Serialize)]
struct Install {
    name: String,
    base64: String
}

async fn install(State(state): State<AppState>, Json(install): Json<Install>) -> Result<Json<()>, AppError> {
    let bytes = base64::engine::general_purpose::STANDARD.decode(&install.base64)?;

    if let Err(e) = Runtime::check_wasm(bytes.clone()) {
        Err(anyhow::anyhow!("Invalid WASM bytecode provided: {:?}", e))?;
    }

    let mut registry = state.registry.write().unwrap();

    registry.install(&install.name, bytes)?;

    Ok(Json(()))
}

async fn ps(State(state): State<AppState>) -> Result<Json<Vec<InstanceInfo>>, AppError> {
    let runtime = state.runtime.read().unwrap();

    let instances = runtime.ps();
    Ok(Json(instances))
}

#[derive(Deserialize, Serialize)]
struct Launch {
    package: String,
    args: Vec<String>
}

async fn launch(State(state): State<AppState>, Json(launch): Json<Launch>) -> Result<Json<String>, AppError> {
    let registry = state.registry.read().unwrap();
    let mut runtime = state.runtime.write().unwrap();

    let bytecode = registry.read(launch.package.as_str())?;
    let id = runtime.launch(launch.package, bytecode, launch.args)?;

    Ok(Json(id.to_string()))
}

#[derive(Deserialize, Serialize)]
struct Kill {
    id: String
}

async fn kill(State(state): State<AppState>, Json(kill): Json<Kill>) -> Result<Json<()>, AppError> {
    let runtime = state.runtime.read().unwrap();
    runtime.kill(kill.id.as_str());

    Ok(Json(()))
}

#[derive(Deserialize, Serialize)]
struct Res {
    id: String
}

async fn res(State(state): State<AppState>, Json(res): Json<Res>) -> Result<Json<Option<String>>, AppError> {
    let mut runtime = state.runtime.write().unwrap();
    let result = runtime.res(res.id.as_str());

    Ok(Json(result))
}

async fn attach(State(state): State<AppState>, Path(package): Path<String>, socket: WebSocketUpgrade) -> impl IntoResponse {
    let state_copy = state.clone();
    let registry = state_copy.registry.read().unwrap();
    let mut runtime = state_copy.runtime.write().unwrap();

    let bytecode = registry.read(package.as_str()).unwrap();

    let stdin = Arc::new(RwLock::new(StdioForwarder::new()));
    let stdout = Arc::new(RwLock::new(StdioForwarder::new()));

    let id = runtime.attach(package, bytecode, stdin.clone(), stdout.clone()).unwrap();

    socket.on_upgrade(move |socket| {
        process_attached(
            state,
            socket,
            id.to_string(),
            stdin,
            stdout
        )
    })
}

async fn process_attached(
    state: AppState,
    mut socket: WebSocket,
    id: String,
    stdin: Arc<RwLock<StdioForwarder>>,
    stdout: Arc<RwLock<StdioForwarder>>
) {
    let span = tracing::span!(tracing::Level::DEBUG, "Processing attachment");
    let _ = span.enter();

    loop {
        match socket.recv().await {
            None => {
                tracing::debug!("Socket connection closed");
                break;
            },
            Some(Err(e)) => {
                let log = format!("Socket error receiving data, terminating connection {:?}", e);
                tracing::warn!(log);
                break;
            },
            Some(Ok(msg)) => match msg {
                Message::Close(_) => {
                    tracing::debug!("Socket connection closed");
                    break;
                }
                Message::Text(data) => {
                    if data == ":q" {
                        let runtime = state.runtime.read().unwrap();
                        runtime.kill(id.as_str());

                        let _ = socket.close();

                        break;
                    } else if data != "" {
                        let echo = format!("> {}\n", data);
                        socket.send(Message::Text(echo)).await.unwrap();

                        let data = format!("{}\n", data);
                        stdin.write().unwrap().push(data.as_bytes());
                    }

                    let available = String::from_utf8(stdout.write().unwrap().pull_all()).unwrap();
                    socket.send(Message::Text(available)).await.unwrap();
                },
                _ => {
                    let log = format!("Unsupported message received, terminating connection");
                    tracing::error!(log);
                    break;
                }
            }
        }
    }

    let runtime = state.runtime.read().unwrap();
    runtime.kill(id.as_str());
}
