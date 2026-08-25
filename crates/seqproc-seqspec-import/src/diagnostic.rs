use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Severity {
    Info,
    Warning,
    Error,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum DiagnosticCategory {
    SourceInvalid,
    NeedsSelection,
    UnsupportedCapability,
    ResourceUnresolved,
    LossyNormalization,
    Informational,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ImportDiagnostic {
    pub code: String,
    pub severity: Severity,
    pub category: DiagnosticCategory,
    pub message: String,
    pub yaml_path: Option<String>,
    pub line: Option<usize>,
    pub column: Option<usize>,
    pub modality: Option<String>,
    pub read_id: Option<String>,
    pub capability: Option<String>,
    pub remediation: Option<String>,
}

impl ImportDiagnostic {
    pub(crate) fn error(
        code: impl Into<String>,
        category: DiagnosticCategory,
        message: impl Into<String>,
    ) -> Self {
        Self {
            code: code.into(),
            severity: Severity::Error,
            category,
            message: message.into(),
            yaml_path: None,
            line: None,
            column: None,
            modality: None,
            read_id: None,
            capability: None,
            remediation: None,
        }
    }

    pub(crate) fn warning(
        code: impl Into<String>,
        category: DiagnosticCategory,
        message: impl Into<String>,
    ) -> Self {
        Self {
            code: code.into(),
            severity: Severity::Warning,
            category,
            message: message.into(),
            yaml_path: None,
            line: None,
            column: None,
            modality: None,
            read_id: None,
            capability: None,
            remediation: None,
        }
    }

    pub(crate) fn at(mut self, path: impl Into<String>) -> Self {
        self.yaml_path = Some(path.into());
        self
    }

    pub(crate) fn for_modality(mut self, modality: impl Into<String>) -> Self {
        self.modality = Some(modality.into());
        self
    }

    pub(crate) fn for_read(mut self, read_id: impl Into<String>) -> Self {
        self.read_id = Some(read_id.into());
        self
    }

    pub(crate) fn requiring(mut self, capability: impl Into<String>) -> Self {
        self.capability = Some(capability.into());
        self
    }

    pub(crate) fn remediate(mut self, remediation: impl Into<String>) -> Self {
        self.remediation = Some(remediation.into());
        self
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ImportStatus {
    Supported,
    SupportedRequiresBinding,
    SupportedWithSelection,
    NeedsUserPolicy,
    BlockedSourceInvalid,
    BlockedSeqprocCapability,
    Deferred,
    Rejected,
}

impl ImportStatus {
    pub fn generated(self) -> bool {
        matches!(
            self,
            Self::Supported | Self::SupportedRequiresBinding | Self::SupportedWithSelection
        )
    }
}
