pub struct CodeResult {
  pub code: String,
  pub file_name: String,
}

impl CodeResult {
  pub fn new(code: String, file_name: String) -> Self {
    Self { code, file_name }
  }
}


