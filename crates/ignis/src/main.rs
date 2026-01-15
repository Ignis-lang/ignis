mod cli;

use clap::Parser as ClapParser;
use ignis_driver::{build_std, check_runtime, check_std, compile_project};
use std::{sync::Arc, fs::File, io::Read, path::Path};

use cli::{Cli, SubCommand};
use ignis_config;

fn load_project_config() -> Option<ignis_config::IgnisProjectConfig> {
  let binding = std::env::current_dir().unwrap();
  let file_path = binding.to_str().unwrap();

  let file_path = Path::new(file_path).join("ignis.toml");

  if file_path.exists() {
    let file = File::open(file_path);
    if file.is_err() {
      println!("Failed to open file");
      std::process::exit(1);
    }

    let mut file = file.unwrap();
    let mut content = String::new();

    let _ = file.read_to_string(&mut content);

    let config: ignis_config::IgnisProjectConfig = toml::from_str(&content).unwrap();

    return Some(config);
  }

  return None;
}

fn check_if_project() -> bool {
  let binding = std::env::current_dir().unwrap();
  let file_path = binding.to_str().unwrap();

  let file_path = Path::new(file_path).join("ignis.toml");

  if file_path.exists() {
    let file = File::open(file_path);
    if file.is_err() {
      return false;
    }

    return true;
  }

  false
}

fn load_manifest(std_path: &str) -> ignis_config::IgnisSTDManifest {
  let file_path = Path::new(std_path).join("manifest.toml");

  if file_path.exists() {
    let file = File::open(file_path);
    if file.is_err() {
      println!("Failed to open file");
      std::process::exit(1);
    }

    let mut file = file.unwrap();
    let mut content = String::new();

    let _ = file.read_to_string(&mut content);

    let config: ignis_config::IgnisSTDManifest = toml::from_str(&content).unwrap();

    return config;
  }

  return ignis_config::IgnisSTDManifest::default();
}

fn parse_cli_to_config(cli: &Cli) -> Arc<ignis_config::IgnisConfig> {
  let mut config = ignis_config::IgnisConfig::new_basic(
    cli.debug,
    cli.debug_trace.iter().copied().map(Into::into).collect(),
    cli.quiet,
    cli.verbose,
  );

  config.std = !cli.std;
  config.auto_load_std = !cli.auto_load_std;

  if cli.std_path.eq("IGNIS_STD_PATH") {
    if let Ok(v) = std::env::var("IGNIS_STD_PATH") {
      config.std_path = v;
    }
  } else {
    config.std_path = cli.std_path.clone();
  }

  match &cli.subcommand {
    SubCommand::Build(build) => {
      let is_project = check_if_project();

      if build.file_path.is_none() && !is_project {
        println!("No file path provided. Please provide a file path or run `ignis init` to create a new project.");
        println!("For more information, run `ignis --help`");
        std::process::exit(1);
      }

      // Determine binary output path (default behavior: always produce a binary)
      let emit_bin = if build.lib {
        None
      } else if build.emit_bin.is_some() {
        build.emit_bin.clone()
      } else if let Some(ref file_path) = build.file_path {
        // User specified a file explicitly - use file name without extension
        let path = Path::new(file_path);
        let stem = path.file_stem().and_then(|s| s.to_str()).unwrap_or("out");
        Some(format!("{}/{}", build.output_dir, stem))
      } else if is_project {
        // Building project without explicit file - use project name
        let project_config = load_project_config().unwrap();
        Some(format!("{}/{}", build.output_dir, project_config.name))
      } else {
        Some(format!("{}/out", build.output_dir))
      };

      if is_project {
        let mut project_config = load_project_config().unwrap();

        if config.std_path.is_empty() {
          config.std_path = project_config.ignis.std_path.clone();
        }

        project_config.build.target = build.target.clone().into();

        if build.optimize != project_config.build.optimize {
          project_config.build.optimize = build.optimize;
        }

        if build.output_dir != "build" {
          project_config.build.output_dir = build.output_dir.clone();
        }

        if build.file_path.is_some() {
          project_config.build.main_file = build.file_path.as_ref().unwrap().to_string();
        }

        config.project_config = Some(project_config);
      }

      let entry_file = if build.file_path.is_some() {
        build.file_path.clone()
      } else if let Some(project_config) = &config.project_config {
        Some(project_config.build.main_file.clone())
      } else {
        None
      };

      config.build = true;
      config
        .build_config
        .clone_from(&Some(ignis_config::IgnisBuildConfig::new(
          entry_file,
          build.target.clone().into(),
          is_project,
          build.optimize.clone(),
          build.output_dir.clone(),
          cli.dump.iter().copied().map(Into::into).collect(),
          cli.dump_dir.clone(),
          cli.dump_hir.clone(),
          build.emit_c.clone(),
          build.emit_obj.clone(),
          emit_bin,
          build.rebuild_std,
          build.bin,
          build.lib,
          false,
          false,
        )));
    },
    SubCommand::Check(check) => {
      let is_project = check_if_project();

      if check.file_path.is_none() && !is_project {
        println!("No file path provided. Please provide a file path or run `ignis init` to create a new project.");
        println!("For more information, run `ignis --help`");
        std::process::exit(1);
      }

      if is_project {
        let mut project_config = load_project_config().unwrap();

        if config.std_path.is_empty() {
          config.std_path = project_config.ignis.std_path.clone();
        }

        project_config.build.target = check.target.clone().into();

        if check.output_dir != "build" {
          project_config.build.output_dir = check.output_dir.clone();
        }

        if check.file_path.is_some() {
          project_config.build.main_file = check.file_path.as_ref().unwrap().to_string();
        }

        config.project_config = Some(project_config);
      }

      let entry_file = if check.file_path.is_some() {
        check.file_path.clone()
      } else if let Some(project_config) = &config.project_config {
        Some(project_config.build.main_file.clone())
      } else {
        None
      };

      config.build = true;
      config
        .build_config
        .clone_from(&Some(ignis_config::IgnisBuildConfig::new(
          entry_file,
          check.target.clone().into(),
          is_project,
          false,
          check.output_dir.clone(),
          cli.dump.iter().copied().map(Into::into).collect(),
          cli.dump_dir.clone(),
          cli.dump_hir.clone(),
          check.emit_c.clone(),
          None,
          None,
          false,
          false,
          false,
          true,
          check.analyze_only,
        )));
    },
    SubCommand::Init(init) => {
      config.init = true;
      config.init_config.clone_from(&Some(ignis_config::IgnisInitConfig::new(
        init.name.clone(),
        init.project_version.clone(),
        init.authors.clone(),
        init.description.clone(),
        init.keywords.clone(),
        init.license.clone(),
        init.repository.clone(),
        init.git,
        init.target.clone().into(),
      )));
    },
    SubCommand::BuildStd(build_std) => {
      config.build_std = true;
      config.build_std_output_dir = Some(build_std.output_dir.clone());
    },
    SubCommand::CheckStd(check_std) => {
      config.check_std = true;
      config.build_std_output_dir = Some(check_std.output_dir.clone());
    },
    SubCommand::CheckRuntime(check_runtime) => {
      config.check_runtime = true;
      config.runtime_path_override = check_runtime.runtime_path.clone();
    },
  };

  config.manifest = load_manifest(&config.std_path);

  Arc::new(config)
}

fn main() {
  let cli = Cli::parse();

  let config = parse_cli_to_config(&cli);

  if config.check_std {
    let output_dir = config.build_std_output_dir.as_deref().unwrap_or("build");
    match check_std(config.clone(), output_dir) {
      Ok(()) => {},
      Err(()) => std::process::exit(1),
    }
    return;
  }

  if config.check_runtime {
    let runtime_path = config.runtime_path_override.as_deref();
    match check_runtime(config.clone(), runtime_path) {
      Ok(()) => {},
      Err(()) => std::process::exit(1),
    }
    return;
  }

  if config.build_std {
    let output_dir = config.build_std_output_dir.as_ref().unwrap();
    match build_std(config.clone(), output_dir) {
      Ok(()) => {},
      Err(()) => std::process::exit(1),
    }
    return;
  }

  if config.build {
    let build_config = config.build_config.clone().unwrap();
    let file_path = build_config.file.unwrap();

    match compile_project(config, &file_path) {
      Ok(()) => {},
      Err(()) => std::process::exit(1),
    }
    return;
  }

  if config.init {
    // TODO: Implement init command
    println!("Init command not yet implemented");
    return;
  }
}
