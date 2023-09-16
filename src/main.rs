use axum::{Router, Server};
use axum::body::Body;
use axum::extract::{Json, State};
use axum::http::Request;
use axum::routing::{get, post};
use base64::Engine;
use clap::Parser;
use lykos::error::AppError;
use lykos::registry::{Package, Registry};
use serde::{Deserialize, Serialize};
use std::sync::{Arc, RwLock};
use tower::ServiceExt;
use tower_http::services::{ServeDir, ServeFile};

#[derive(Parser)]
struct Args {
    #[arg(long = "root")]
    root: String
}

#[derive(Clone)]
struct AppState {
    registry: Arc<RwLock<Registry>>
}

#[tokio::main]
async fn main() {
    let args = Args::parse();

    let registry = Registry::new(&args.root).expect("couldnt instantiate registry");
    let state = AppState {
        registry: Arc::new(RwLock::new(registry))
    };

    let spa = ServeDir::new("static")
        .not_found_service(ServeFile::new("static/index.html"));

    let app = Router::new()
        .route("/ls", post(ls))
        .route("/rm", post(rm))
        .route("/install", post(install))
        .nest_service(
            "/assets",
            get(move |request: Request<Body>| async {
                spa.oneshot(request).await
            })
        )
        .fallback_service(ServeFile::new("static/index.html"))
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

    let mut registry = state.registry.write().unwrap();

    registry.install(&install.name, bytes)?;

    Ok(Json(()))
}
