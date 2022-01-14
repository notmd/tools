use super::{ExtensionHandler, Mime};
#[derive(Debug, PartialEq, Eq)]
pub struct JsonFileHandler {}

impl ExtensionHandler for JsonFileHandler {
    fn capabilities(&self) -> super::Capabilities {
        super::Capabilities {
            format: true,
            lint: true,
        }
    }

    fn language(&self) -> super::Language {
        super::Language::Json
    }

    fn mime(&self) -> super::Mime {
        Mime::Json
    }

    fn may_use_tabs(&self) -> bool {
        true
    }
}
