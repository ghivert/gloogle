# Gloogle

Gloogle is a search engine, able to query the entirety of the Gleam ecosystem!
Search for a function or a type, and let the magic happen!

[First beta version is available!](https://gloogle.run)

## Features

Some features are already implemented, and some others are still on the roadmap!

- Search for a function or a type by its name
- Search for a function or a type by its type
- Search with a document search accross the gleam ecosystem (with vector search)
- Navigate in the documentation by clicking on types
- Head back to [HexDocs](https://hexdocs.pm) when looking for something specific
- Find a way to expose the database
- Get GitHub stars for each package on a regular basis
- Enables Search in documentation directly
- Filters included with `in:module`, `in:name`, `in:signature` and
  `in:documentation`

## Future developments

- Use a proper parser to draw a graph of the function calls
- Add analytics to extract trends
- Get Hex data for each package on a regular basis
- Do something with the GitHub stars

<!-- Is it really useful? -->
<!-- - Implements a full-mirror of hex, in case hex is down (because we can, so why not) -->
<!-- - Add a way to visualise a package directly -->

## Running Locally

### Requirements

To run the app locally you must have the following dependencies:

- [mise](https://mise.jdx.dev)
- [yarn](https://yarnpkg.com)
- [Docker](https://www.docker.comv)
- [dbmate](https://github.com/amacneil/dbmate)

Other dependencies (i.e. Gleam, Erlang, etc...) can be installed by running `mise install` in the root of the project.

### Setup Instructions

- Set the following environment variables:
  - `HEX_API_KEY`
  - `GITHUB_TOKEN`
  - `PORT=3000`
- Setup the database by running the following commands:
  - `cd ./apps/backend`
  - `deno task db:init`
  - `dbmate migrate`
- In the root of the project run:
  - `yarn install`
  - `yarn dev`

## Contributing

If Gloogle please you, you can also contribute! Pull Requests are welcome!

## An issue?

Open an issue on the repo! That would be extremely helpful!

## Copyrights

All rights for Lucy (the gleam mascott) belongs to their creators. Every Lucy
will be removed upon demands.

This project can have lived thanks to Louis Pilfold, who did an amazing work on
[Gleam Packages](https://github.com/gleam-lang/packages), that has been used as
a foundation. All credits for libraries used goes to the brilliant, vibrant
gleam community. 💜
