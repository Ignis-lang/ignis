mod cli;

use clap::Parser as ClapParser;
use std::{fs::File, io::Read, path::Path};

use cli::{Cli, SubCommand};
use ignis_core::IgnisCore;
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

  return false;
}

fn parse_cli_to_config(cli: &Cli) -> ignis_config::IgnisConfig {
  let mut config = ignis_config::IgnisConfig::new_basic(
    cli.debug.iter().map(|x| x.into()).collect(),
    cli.quiet,
    cli.verbose,
    cli.std_path.clone(),
  );

  if config.std_path == "IGNIS_STD_PATH" {
    if let Ok(v) = std::env::var("IGNIS_STD_PATH") {
      config.std_path = v;
    } else {
      println!("Failed to load std path from environment variable");
      std::process::exit(1);
    }
  }

  match &cli.subcommand {
    SubCommand::Build(build) => {
      let is_project = check_if_project();

      if build.file_path.is_none() && !is_project {
        println!("No file path provided. Please provide a file path or run `ignis init` to create a new project.");
        println!("For more information, run `ignis --help`");
        std::process::exit(1);
      }

      if is_project {
        let mut project_config = load_project_config().unwrap();

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

      config.build = true;
      config
        .build_config
        .clone_from(&Some(ignis_config::IgnisBuildConfig::new(
          build.file_path.clone(),
          build.target.clone().into(),
          is_project,
          build.optimize.clone(),
          build.output_dir.clone(),
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
  };

  return config;
}

fn main() {
  let cli = Cli::parse();

  let config = parse_cli_to_config(&cli);

  let mut core = IgnisCore::new(&config);

  core.run();
}
