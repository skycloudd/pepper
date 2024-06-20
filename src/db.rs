use owo_colors::OwoColorize as _;
use salsa::DebugWithDb as _;
use std::sync::{Arc, Mutex};

#[derive(Default)]
#[salsa::db(crate::Jar)]
pub struct Database {
    storage: salsa::Storage<Self>,

    logs: Option<Arc<Mutex<Vec<String>>>>,
}

impl salsa::Database for Database {
    fn salsa_event(&self, event: salsa::Event) {
        eprintln!("Event: {:?}", event.dimmed());

        if let Some(logs) = &self.logs {
            if let salsa::EventKind::WillExecute { .. } = event.kind {
                logs.lock()
                    .unwrap()
                    .push(format!("Event: {:?}", event.debug(self)));
            }
        }
    }
}

impl salsa::ParallelDatabase for Database {
    fn snapshot(&self) -> salsa::Snapshot<Self> {
        salsa::Snapshot::new(Self {
            storage: self.storage.snapshot(),
            logs: self.logs.clone(),
        })
    }
}
