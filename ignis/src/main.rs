mod cli;

use clap::Parser as ClapParser;
use std::{fs::File, path::Path};

use cli::{Cli, SubCommand};
use ignis_config;

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
  let mut config =
    ignis_config::IgnisConfig::new_basic(cli.debug.iter().map(|x| x.into()).collect(), cli.quiet, cli.verbose);

  match &cli.subcommand {
    SubCommand::Build(build) => {
      let is_project = check_if_project();

      if build.file_path.is_none() && !is_project {
        println!("No file path provided. Please provide a file path or run `ignis init` to create a new project.");
        println!("For more information, run `ignis --help`");
        std::process::exit(1);
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

  println!("{:#?}", config);
}
