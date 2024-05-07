import data/decoders/search_result
import gleam/json

pub fn mock() {
  "[
    {
        \"name\": \"App\",
        \"documentation\": \"\",
        \"nature\": \"type_definition\",
        \"metadata\": {
            \"deprecation\": null
        },
        \"json_signature\": {
            \"name\": \"App\",
            \"type\": \"type-definition\",
            \"parameters\": 3,
            \"deprecation\": \"\\n\",
            \"constructors\": [],
            \"documentation\": \"\\n\"
        },
        \"module_name\": \"lustre\",
        \"package_name\": \"lustre\",
        \"version\": \"3.1.1\"
    },
    {
        \"name\": \"bg_app_subtle\",
        \"documentation\": \"\",
        \"nature\": \"function\",
        \"metadata\": {
            \"deprecation\": null,
            \"implementations\": {
                \"gleam\": false,
                \"uses_erlang_externals\": true,
                \"uses_javascript_externals\": true
            }
        },
        \"json_signature\": {
            \"name\": \"bg_app_subtle\",
            \"type\": \"function\",
            \"return\": {
                \"ref\": \"3.1.4\",
                \"name\": \"Attribute\",
                \"type\": \"named\",
                \"module\": \"lustre/attribute\",
                \"package\": \"lustre\",
                \"parameters\": [
                    {
                        \"id\": 0,
                        \"type\": \"variable\"
                    }
                ]
            },
            \"parameters\": [],
            \"deprecation\": null,
            \"documentation\": null,
            \"implementations\": {
                \"gleam\": false,
                \"uses_erlang_externals\": true,
                \"uses_javascript_externals\": true
            }
        },
        \"module_name\": \"lustre/ui/classes\",
        \"package_name\": \"lustre_ui\",
        \"version\": \"0.3.0\"
    },
    {
        \"name\": \"App\",
        \"documentation\": \"Represents a constructed Lustre application that is ready to be started.\\n Depending on where you want the application to run, you have a few options:\\n\\n - Use [`start`](#start) to start a single-page-application in the browser.\\n\\n   This is the most common way to start a Lustre application. If you're new to\\n   Lustre or frontend development in general, make sure you check out the\\n   [examples](https://github.com/lustre-labs/lustre/tree/main/examples) or the\\n   [quickstart guide]()\\n\\n - Use [`start_server_component`](#start_server_component) to start a Lustre\\n   Server Component anywhere Gleam will run: Erlang, Node, Deno, or in the\\n   browser. If you're running on the BEAM though, you should...\\n\\n - Use [`start_actor`](#start_actor) to start a Lustre Server Component only\\n   for the Erlang target. BEAM users should always prefer this over\\n   `start_server_component` so they can take advantage of OTP features.\\n\\n - Use [`register`](#register) to register a component in the browser to be\\n   used as a Custom Element. This is useful even if you're not using Lustre\\n   to build a SPA.\\n\\n If you're only interested in using Lustre as a HTML templating engine, you\\n don't need an `App` at all! You can render an element directly using the\\n [`element.to_string`](./lustre/element#to_string) function.\",
        \"nature\": \"type_definition\",
        \"metadata\": {
            \"deprecation\": null
        },
        \"json_signature\": {
            \"name\": \"App\",
            \"type\": \"type-definition\",
            \"parameters\": 3,
            \"deprecation\": \" Represents a constructed Lustre application that is ready to be started.\\n Depending on where you want the application to run, you have a few options:\\n\\n - Use [`start`](#start) to start a single-page-application in the browser.\\n\\n   This is the most common way to start a Lustre application. If you're new to\\n   Lustre or frontend development in general, make sure you check out the\\n   [examples](https://github.com/lustre-labs/lustre/tree/main/examples) or the\\n   [quickstart guide]()\\n\\n - Use [`start_server_component`](#start_server_component) to start a Lustre\\n   Server Component anywhere Gleam will run: Erlang, Node, Deno, or in the\\n   browser. If you're running on the BEAM though, you should...\\n\\n - Use [`start_actor`](#start_actor) to start a Lustre Server Component only\\n   for the Erlang target. BEAM users should always prefer this over\\n   `start_server_component` so they can take advantage of OTP features.\\n\\n - Use [`register`](#register) to register a component in the browser to be\\n   used as a Custom Element. This is useful even if you're not using Lustre\\n   to build a SPA.\\n\\n If you're only interested in using Lustre as a HTML templating engine, you\\n don't need an `App` at all! You can render an element directly using the\\n [`element.to_string`](./lustre/element#to_string) function.\\n\",
            \"constructors\": [],
            \"documentation\": \" Represents a constructed Lustre application that is ready to be started.\\n Depending on where you want the application to run, you have a few options:\\n\\n - Use [`start`](#start) to start a single-page-application in the browser.\\n\\n   This is the most common way to start a Lustre application. If you're new to\\n   Lustre or frontend development in general, make sure you check out the\\n   [examples](https://github.com/lustre-labs/lustre/tree/main/examples) or the\\n   [quickstart guide]()\\n\\n - Use [`start_server_component`](#start_server_component) to start a Lustre\\n   Server Component anywhere Gleam will run: Erlang, Node, Deno, or in the\\n   browser. If you're running on the BEAM though, you should...\\n\\n - Use [`start_actor`](#start_actor) to start a Lustre Server Component only\\n   for the Erlang target. BEAM users should always prefer this over\\n   `start_server_component` so they can take advantage of OTP features.\\n\\n - Use [`register`](#register) to register a component in the browser to be\\n   used as a Custom Element. This is useful even if you're not using Lustre\\n   to build a SPA.\\n\\n If you're only interested in using Lustre as a HTML templating engine, you\\n don't need an `App` at all! You can render an element directly using the\\n [`element.to_string`](./lustre/element#to_string) function.\\n\"
        },
        \"module_name\": \"lustre\",
        \"package_name\": \"lustre\",
        \"version\": \"4.1.3\"
    },
    {
        \"name\": \"Scale\",
        \"documentation\": \"\",
        \"nature\": \"type_definition\",
        \"metadata\": {
            \"deprecation\": null
        },
        \"json_signature\": {
            \"name\": \"Scale\",
            \"type\": \"type-definition\",
            \"parameters\": 0,
            \"deprecation\": null,
            \"constructors\": [
                {
                    \"name\": \"Scale\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"app_background\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"app_background_subtle\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"app_border\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"element_background\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"element_background_hover\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"element_background_strong\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"element_border_subtle\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"element_border_strong\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"solid_background\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"solid_background_hover\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"text_high_contrast\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"text_low_contrast\"
                        }
                    ],
                    \"documentation\": null
                }
            ],
            \"documentation\": null
        },
        \"module_name\": \"lustre/ui/colour\",
        \"package_name\": \"lustre_ui\",
        \"version\": \"0.2.1\"
    },
    {
        \"name\": \"App\",
        \"documentation\": \"Represents a constructed Lustre application that is ready to be started.\\n Depending on where you want the application to run, you have a few options:\\n\\n - Use [`start`](#start) to start a single-page-application in the browser.\\n\\n   This is the most common way to start a Lustre application. If you're new to\\n   Lustre or frontend development in general, make sure you check out the\\n   [examples](https://github.com/lustre-labs/lustre/tree/main/examples) or the\\n   [quickstart guide]()\\n\\n - Use [`start_server_component`](#start_server_component) to start a Lustre\\n   Server Component anywhere Gleam will run: Erlang, Node, Deno, or in the\\n   browser. If you're running on the BEAM though, you should...\\n\\n - Use [`start_actor`](#start_actor) to start a Lustre Server Component only\\n   for the Erlang target. BEAM users should always prefer this over\\n   `start_server_component` so they can take advantage of OTP features.\\n\\n - Use [`register`](#register) to register a component in the browser to be\\n   used as a Custom Element. This is useful even if you're not using Lustre\\n   to build a SPA.\\n\\n If you're only interested in using Lustre as a HTML templating engine, you\\n don't need an `App` at all! You can render an element directly using the\\n [`element.to_string`](./lustre/element#to_string) function.\",
        \"nature\": \"type_definition\",
        \"metadata\": {
            \"deprecation\": null
        },
        \"json_signature\": {
            \"name\": \"App\",
            \"type\": \"type-definition\",
            \"parameters\": 3,
            \"deprecation\": \" Represents a constructed Lustre application that is ready to be started.\\n Depending on where you want the application to run, you have a few options:\\n\\n - Use [`start`](#start) to start a single-page-application in the browser.\\n\\n   This is the most common way to start a Lustre application. If you're new to\\n   Lustre or frontend development in general, make sure you check out the\\n   [examples](https://github.com/lustre-labs/lustre/tree/main/examples) or the\\n   [quickstart guide]()\\n\\n - Use [`start_server_component`](#start_server_component) to start a Lustre\\n   Server Component anywhere Gleam will run: Erlang, Node, Deno, or in the\\n   browser. If you're running on the BEAM though, you should...\\n\\n - Use [`start_actor`](#start_actor) to start a Lustre Server Component only\\n   for the Erlang target. BEAM users should always prefer this over\\n   `start_server_component` so they can take advantage of OTP features.\\n\\n - Use [`register`](#register) to register a component in the browser to be\\n   used as a Custom Element. This is useful even if you're not using Lustre\\n   to build a SPA.\\n\\n If you're only interested in using Lustre as a HTML templating engine, you\\n don't need an `App` at all! You can render an element directly using the\\n [`element.to_string`](./lustre/element#to_string) function.\\n\",
            \"constructors\": [],
            \"documentation\": \" Represents a constructed Lustre application that is ready to be started.\\n Depending on where you want the application to run, you have a few options:\\n\\n - Use [`start`](#start) to start a single-page-application in the browser.\\n\\n   This is the most common way to start a Lustre application. If you're new to\\n   Lustre or frontend development in general, make sure you check out the\\n   [examples](https://github.com/lustre-labs/lustre/tree/main/examples) or the\\n   [quickstart guide]()\\n\\n - Use [`start_server_component`](#start_server_component) to start a Lustre\\n   Server Component anywhere Gleam will run: Erlang, Node, Deno, or in the\\n   browser. If you're running on the BEAM though, you should...\\n\\n - Use [`start_actor`](#start_actor) to start a Lustre Server Component only\\n   for the Erlang target. BEAM users should always prefer this over\\n   `start_server_component` so they can take advantage of OTP features.\\n\\n - Use [`register`](#register) to register a component in the browser to be\\n   used as a Custom Element. This is useful even if you're not using Lustre\\n   to build a SPA.\\n\\n If you're only interested in using Lustre as a HTML templating engine, you\\n don't need an `App` at all! You can render an element directly using the\\n [`element.to_string`](./lustre/element#to_string) function.\\n\"
        },
        \"module_name\": \"lustre\",
        \"package_name\": \"lustre\",
        \"version\": \"4.0.0\"
    },
    {
        \"name\": \"Scale\",
        \"documentation\": \"\",
        \"nature\": \"type_definition\",
        \"metadata\": {
            \"deprecation\": null
        },
        \"json_signature\": {
            \"name\": \"Scale\",
            \"type\": \"type-definition\",
            \"parameters\": 0,
            \"deprecation\": null,
            \"constructors\": [
                {
                    \"name\": \"Scale\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"app_background\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"app_background_subtle\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"app_border\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"element_background\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"element_background_hover\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"element_background_strong\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"element_border_subtle\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"element_border_strong\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"solid_background\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"solid_background_hover\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"text_high_contrast\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"text_low_contrast\"
                        }
                    ],
                    \"documentation\": null
                }
            ],
            \"documentation\": null
        },
        \"module_name\": \"lustre/ui/colour\",
        \"package_name\": \"lustre_ui\",
        \"version\": \"0.5.0\"
    },
    {
        \"name\": \"App\",
        \"documentation\": \"Represents a constructed Lustre application that is ready to be started.\\n Depending on where you want the application to run, you have a few options:\\n\\n - Use [`start`](#start) to start a single-page-application in the browser.\\n\\n   This is the most common way to start a Lustre application. If you're new to\\n   Lustre or frontend development in general, make sure you check out the\\n   [examples](https://github.com/lustre-labs/lustre/tree/main/examples) or the\\n   [quickstart guide]()\\n\\n - Use [`start_server_component`](#start_server_component) to start a Lustre\\n   Server Component anywhere Gleam will run: Erlang, Node, Deno, or in the\\n   browser. If you're running on the BEAM though, you should...\\n\\n - Use [`start_actor`](#start_actor) to start a Lustre Server Component only\\n   for the Erlang target. BEAM users should always prefer this over\\n   `start_server_component` so they can take advantage of OTP features.\\n\\n - Use [`register`](#register) to register a component in the browser to be\\n   used as a Custom Element. This is useful even if you're not using Lustre\\n   to build a SPA.\\n\\n If you're only interested in using Lustre as a HTML templating engine, you\\n don't need an `App` at all! You can render an element directly using the\\n [`element.to_string`](./lustre/element#to_string) function.\",
        \"nature\": \"type_definition\",
        \"metadata\": {
            \"deprecation\": null
        },
        \"json_signature\": {
            \"name\": \"App\",
            \"type\": \"type-definition\",
            \"parameters\": 3,
            \"deprecation\": \" Represents a constructed Lustre application that is ready to be started.\\n Depending on where you want the application to run, you have a few options:\\n\\n - Use [`start`](#start) to start a single-page-application in the browser.\\n\\n   This is the most common way to start a Lustre application. If you're new to\\n   Lustre or frontend development in general, make sure you check out the\\n   [examples](https://github.com/lustre-labs/lustre/tree/main/examples) or the\\n   [quickstart guide]()\\n\\n - Use [`start_server_component`](#start_server_component) to start a Lustre\\n   Server Component anywhere Gleam will run: Erlang, Node, Deno, or in the\\n   browser. If you're running on the BEAM though, you should...\\n\\n - Use [`start_actor`](#start_actor) to start a Lustre Server Component only\\n   for the Erlang target. BEAM users should always prefer this over\\n   `start_server_component` so they can take advantage of OTP features.\\n\\n - Use [`register`](#register) to register a component in the browser to be\\n   used as a Custom Element. This is useful even if you're not using Lustre\\n   to build a SPA.\\n\\n If you're only interested in using Lustre as a HTML templating engine, you\\n don't need an `App` at all! You can render an element directly using the\\n [`element.to_string`](./lustre/element#to_string) function.\\n\",
            \"constructors\": [],
            \"documentation\": \" Represents a constructed Lustre application that is ready to be started.\\n Depending on where you want the application to run, you have a few options:\\n\\n - Use [`start`](#start) to start a single-page-application in the browser.\\n\\n   This is the most common way to start a Lustre application. If you're new to\\n   Lustre or frontend development in general, make sure you check out the\\n   [examples](https://github.com/lustre-labs/lustre/tree/main/examples) or the\\n   [quickstart guide]()\\n\\n - Use [`start_server_component`](#start_server_component) to start a Lustre\\n   Server Component anywhere Gleam will run: Erlang, Node, Deno, or in the\\n   browser. If you're running on the BEAM though, you should...\\n\\n - Use [`start_actor`](#start_actor) to start a Lustre Server Component only\\n   for the Erlang target. BEAM users should always prefer this over\\n   `start_server_component` so they can take advantage of OTP features.\\n\\n - Use [`register`](#register) to register a component in the browser to be\\n   used as a Custom Element. This is useful even if you're not using Lustre\\n   to build a SPA.\\n\\n If you're only interested in using Lustre as a HTML templating engine, you\\n don't need an `App` at all! You can render an element directly using the\\n [`element.to_string`](./lustre/element#to_string) function.\\n\"
        },
        \"module_name\": \"lustre\",
        \"package_name\": \"lustre\",
        \"version\": \"4.1.6\"
    },
    {
        \"name\": \"Scale\",
        \"documentation\": \"\",
        \"nature\": \"type_definition\",
        \"metadata\": {
            \"deprecation\": null
        },
        \"json_signature\": {
            \"name\": \"Scale\",
            \"type\": \"type-definition\",
            \"parameters\": 0,
            \"deprecation\": null,
            \"constructors\": [
                {
                    \"name\": \"Scale\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"app_background\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"app_background_subtle\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"app_border\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"element_background\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"element_background_hover\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"element_background_strong\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"element_border_subtle\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"element_border_strong\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"solid_background\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"solid_background_hover\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"text_high_contrast\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"text_low_contrast\"
                        }
                    ],
                    \"documentation\": null
                }
            ],
            \"documentation\": null
        },
        \"module_name\": \"lustre/ui/colour\",
        \"package_name\": \"lustre_ui\",
        \"version\": \"0.3.0\"
    },
    {
        \"name\": \"KeyboardButton\",
        \"documentation\": \"\",
        \"nature\": \"type_definition\",
        \"metadata\": {
            \"deprecation\": null
        },
        \"json_signature\": {
            \"name\": \"KeyboardButton\",
            \"type\": \"type-definition\",
            \"parameters\": 0,
            \"deprecation\": null,
            \"constructors\": [
                {
                    \"name\": \"KeyboardButton\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": {
                                \"ref\": null,
                                \"name\": \"String\",
                                \"type\": \"named\",
                                \"module\": \"gleam\",
                                \"package\": \"\",
                                \"parameters\": []
                            },
                            \"label\": \"text\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": \"0.1.1\",
                                        \"name\": \"KeyboardButtonRequestUsers\",
                                        \"type\": \"named\",
                                        \"module\": \"telega/model\",
                                        \"package\": \"telega\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"request_users\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": \"0.1.1\",
                                        \"name\": \"KeyboardButtonRequestChat\",
                                        \"type\": \"named\",
                                        \"module\": \"telega/model\",
                                        \"package\": \"telega\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"request_chat\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"Bool\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"request_contact\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"Bool\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"request_location\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": \"0.1.1\",
                                        \"name\": \"KeyboardButtonPollType\",
                                        \"type\": \"named\",
                                        \"module\": \"telega/model\",
                                        \"package\": \"telega\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"request_poll\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": \"0.1.1\",
                                        \"name\": \"WebAppInfo\",
                                        \"type\": \"named\",
                                        \"module\": \"telega/model\",
                                        \"package\": \"telega\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"web_app\"
                        }
                    ],
                    \"documentation\": \" This object represents one button of the reply keyboard. For simple text buttons, String can be used instead of this object to specify the button text. The optional fields _web_app_, _request_users_, _request_chat_, _request_contact_, _request_location_, and _request_poll_ are mutually exclusive.\\n\\n **Official reference:** https://core.telegram.org/bots/api#keyboardbutton\"
                }
            ],
            \"documentation\": null
        },
        \"module_name\": \"telega/model\",
        \"package_name\": \"telega\",
        \"version\": \"0.1.1\"
    },
    {
        \"name\": \"border_app\",
        \"documentation\": \"\",
        \"nature\": \"function\",
        \"metadata\": {
            \"deprecation\": null,
            \"implementations\": {
                \"gleam\": false,
                \"uses_erlang_externals\": true,
                \"uses_javascript_externals\": true
            }
        },
        \"json_signature\": {
            \"name\": \"border_app\",
            \"type\": \"function\",
            \"return\": {
                \"ref\": \"3.1.4\",
                \"name\": \"Attribute\",
                \"type\": \"named\",
                \"module\": \"lustre/attribute\",
                \"package\": \"lustre\",
                \"parameters\": [
                    {
                        \"id\": 0,
                        \"type\": \"variable\"
                    }
                ]
            },
            \"parameters\": [],
            \"deprecation\": null,
            \"documentation\": null,
            \"implementations\": {
                \"gleam\": false,
                \"uses_erlang_externals\": true,
                \"uses_javascript_externals\": true
            }
        },
        \"module_name\": \"lustre/ui/classes\",
        \"package_name\": \"lustre_ui\",
        \"version\": \"0.2.2\"
    },
    {
        \"name\": \"App\",
        \"documentation\": \"Represents a constructed Lustre application that is ready to be started.\\n Depending on where you want the application to run, you have a few options:\\n\\n - Use [`start`](#start) to start a single-page-application in the browser.\\n\\n   This is the most common way to start a Lustre application. If you're new to\\n   Lustre or frontend development in general, make sure you check out the\\n   [examples](https://github.com/lustre-labs/lustre/tree/main/examples) or the\\n   [quickstart guide]()\\n\\n - Use [`start_server_component`](#start_server_component) to start a Lustre\\n   Server Component anywhere Gleam will run: Erlang, Node, Deno, or in the\\n   browser. If you're running on the BEAM though, you should...\\n\\n - Use [`start_actor`](#start_actor) to start a Lustre Server Component only\\n   for the Erlang target. BEAM users should always prefer this over\\n   `start_server_component` so they can take advantage of OTP features.\\n\\n - Use [`register`](#register) to register a component in the browser to be\\n   used as a Custom Element. This is useful even if you're not using Lustre\\n   to build a SPA.\\n\\n If you're only interested in using Lustre as a HTML templating engine, you\\n don't need an `App` at all! You can render an element directly using the\\n [`element.to_string`](./lustre/element#to_string) function.\",
        \"nature\": \"type_definition\",
        \"metadata\": {
            \"deprecation\": null
        },
        \"json_signature\": {
            \"name\": \"App\",
            \"type\": \"type-definition\",
            \"parameters\": 3,
            \"deprecation\": \" Represents a constructed Lustre application that is ready to be started.\\n Depending on where you want the application to run, you have a few options:\\n\\n - Use [`start`](#start) to start a single-page-application in the browser.\\n\\n   This is the most common way to start a Lustre application. If you're new to\\n   Lustre or frontend development in general, make sure you check out the\\n   [examples](https://github.com/lustre-labs/lustre/tree/main/examples) or the\\n   [quickstart guide]()\\n\\n - Use [`start_server_component`](#start_server_component) to start a Lustre\\n   Server Component anywhere Gleam will run: Erlang, Node, Deno, or in the\\n   browser. If you're running on the BEAM though, you should...\\n\\n - Use [`start_actor`](#start_actor) to start a Lustre Server Component only\\n   for the Erlang target. BEAM users should always prefer this over\\n   `start_server_component` so they can take advantage of OTP features.\\n\\n - Use [`register`](#register) to register a component in the browser to be\\n   used as a Custom Element. This is useful even if you're not using Lustre\\n   to build a SPA.\\n\\n If you're only interested in using Lustre as a HTML templating engine, you\\n don't need an `App` at all! You can render an element directly using the\\n [`element.to_string`](./lustre/element#to_string) function.\\n\",
            \"constructors\": [],
            \"documentation\": \" Represents a constructed Lustre application that is ready to be started.\\n Depending on where you want the application to run, you have a few options:\\n\\n - Use [`start`](#start) to start a single-page-application in the browser.\\n\\n   This is the most common way to start a Lustre application. If you're new to\\n   Lustre or frontend development in general, make sure you check out the\\n   [examples](https://github.com/lustre-labs/lustre/tree/main/examples) or the\\n   [quickstart guide]()\\n\\n - Use [`start_server_component`](#start_server_component) to start a Lustre\\n   Server Component anywhere Gleam will run: Erlang, Node, Deno, or in the\\n   browser. If you're running on the BEAM though, you should...\\n\\n - Use [`start_actor`](#start_actor) to start a Lustre Server Component only\\n   for the Erlang target. BEAM users should always prefer this over\\n   `start_server_component` so they can take advantage of OTP features.\\n\\n - Use [`register`](#register) to register a component in the browser to be\\n   used as a Custom Element. This is useful even if you're not using Lustre\\n   to build a SPA.\\n\\n If you're only interested in using Lustre as a HTML templating engine, you\\n don't need an `App` at all! You can render an element directly using the\\n [`element.to_string`](./lustre/element#to_string) function.\\n\"
        },
        \"module_name\": \"lustre\",
        \"package_name\": \"lustre\",
        \"version\": \"4.1.1\"
    },
    {
        \"name\": \"App\",
        \"documentation\": \"A Vue App\",
        \"nature\": \"type_definition\",
        \"metadata\": {
            \"deprecation\": null
        },
        \"json_signature\": {
            \"name\": \"App\",
            \"type\": \"type-definition\",
            \"parameters\": 0,
            \"deprecation\": \" A Vue App\",
            \"constructors\": [],
            \"documentation\": \" A Vue App\"
        },
        \"module_name\": \"vleam/vue\",
        \"package_name\": \"vleam\",
        \"version\": \"0.1.0\"
    },
    {
        \"name\": \"App\",
        \"documentation\": \"Represents a constructed Lustre application that is ready to be started.\\n Depending on where you want the application to run, you have a few options:\\n\\n - Use [`start`](#start) to start a single-page-application in the browser.\\n\\n   This is the most common way to start a Lustre application. If you're new to\\n   Lustre or frontend development in general, make sure you check out the\\n   [examples](https://github.com/lustre-labs/lustre/tree/main/examples) or the\\n   [quickstart guide]()\\n\\n - Use [`start_server_component`](#start_server_component) to start a Lustre\\n   Server Component anywhere Gleam will run: Erlang, Node, Deno, or in the\\n   browser. If you're running on the BEAM though, you should...\\n\\n - Use [`start_actor`](#start_actor) to start a Lustre Server Component only\\n   for the Erlang target. BEAM users should always prefer this over\\n   `start_server_component` so they can take advantage of OTP features.\\n\\n - Use [`register`](#register) to register a component in the browser to be\\n   used as a Custom Element. This is useful even if you're not using Lustre\\n   to build a SPA.\\n\\n If you're only interested in using Lustre as a HTML templating engine, you\\n don't need an `App` at all! You can render an element directly using the\\n [`element.to_string`](./lustre/element.html#to_string) function.\",
        \"nature\": \"type_definition\",
        \"metadata\": {
            \"deprecation\": null
        },
        \"json_signature\": {
            \"name\": \"App\",
            \"type\": \"type-definition\",
            \"parameters\": 3,
            \"deprecation\": \" Represents a constructed Lustre application that is ready to be started.\\n Depending on where you want the application to run, you have a few options:\\n\\n - Use [`start`](#start) to start a single-page-application in the browser.\\n\\n   This is the most common way to start a Lustre application. If you're new to\\n   Lustre or frontend development in general, make sure you check out the\\n   [examples](https://github.com/lustre-labs/lustre/tree/main/examples) or the\\n   [quickstart guide]()\\n\\n - Use [`start_server_component`](#start_server_component) to start a Lustre\\n   Server Component anywhere Gleam will run: Erlang, Node, Deno, or in the\\n   browser. If you're running on the BEAM though, you should...\\n\\n - Use [`start_actor`](#start_actor) to start a Lustre Server Component only\\n   for the Erlang target. BEAM users should always prefer this over\\n   `start_server_component` so they can take advantage of OTP features.\\n\\n - Use [`register`](#register) to register a component in the browser to be\\n   used as a Custom Element. This is useful even if you're not using Lustre\\n   to build a SPA.\\n\\n If you're only interested in using Lustre as a HTML templating engine, you\\n don't need an `App` at all! You can render an element directly using the\\n [`element.to_string`](./lustre/element.html#to_string) function.\\n\",
            \"constructors\": [],
            \"documentation\": \" Represents a constructed Lustre application that is ready to be started.\\n Depending on where you want the application to run, you have a few options:\\n\\n - Use [`start`](#start) to start a single-page-application in the browser.\\n\\n   This is the most common way to start a Lustre application. If you're new to\\n   Lustre or frontend development in general, make sure you check out the\\n   [examples](https://github.com/lustre-labs/lustre/tree/main/examples) or the\\n   [quickstart guide]()\\n\\n - Use [`start_server_component`](#start_server_component) to start a Lustre\\n   Server Component anywhere Gleam will run: Erlang, Node, Deno, or in the\\n   browser. If you're running on the BEAM though, you should...\\n\\n - Use [`start_actor`](#start_actor) to start a Lustre Server Component only\\n   for the Erlang target. BEAM users should always prefer this over\\n   `start_server_component` so they can take advantage of OTP features.\\n\\n - Use [`register`](#register) to register a component in the browser to be\\n   used as a Custom Element. This is useful even if you're not using Lustre\\n   to build a SPA.\\n\\n If you're only interested in using Lustre as a HTML templating engine, you\\n don't need an `App` at all! You can render an element directly using the\\n [`element.to_string`](./lustre/element.html#to_string) function.\\n\"
        },
        \"module_name\": \"lustre\",
        \"package_name\": \"lustre\",
        \"version\": \"4.2.0\"
    },
    {
        \"name\": \"bg_app\",
        \"documentation\": \"\",
        \"nature\": \"function\",
        \"metadata\": {
            \"deprecation\": null,
            \"implementations\": {
                \"gleam\": false,
                \"uses_erlang_externals\": true,
                \"uses_javascript_externals\": true
            }
        },
        \"json_signature\": {
            \"name\": \"bg_app\",
            \"type\": \"function\",
            \"return\": {
                \"ref\": \"3.1.4\",
                \"name\": \"Attribute\",
                \"type\": \"named\",
                \"module\": \"lustre/attribute\",
                \"package\": \"lustre\",
                \"parameters\": [
                    {
                        \"id\": 0,
                        \"type\": \"variable\"
                    }
                ]
            },
            \"parameters\": [],
            \"deprecation\": null,
            \"documentation\": null,
            \"implementations\": {
                \"gleam\": false,
                \"uses_erlang_externals\": true,
                \"uses_javascript_externals\": true
            }
        },
        \"module_name\": \"lustre/ui/classes\",
        \"package_name\": \"lustre_ui\",
        \"version\": \"0.2.2\"
    },
    {
        \"name\": \"Scale\",
        \"documentation\": \"\",
        \"nature\": \"type_definition\",
        \"metadata\": {
            \"deprecation\": null
        },
        \"json_signature\": {
            \"name\": \"Scale\",
            \"type\": \"type-definition\",
            \"parameters\": 0,
            \"deprecation\": null,
            \"constructors\": [
                {
                    \"name\": \"Scale\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"app_background\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"app_background_subtle\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"app_border\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"element_background\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"element_background_hover\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"element_background_strong\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"element_border_subtle\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"element_border_strong\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"solid_background\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"solid_background_hover\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"text_high_contrast\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"text_low_contrast\"
                        }
                    ],
                    \"documentation\": null
                }
            ],
            \"documentation\": null
        },
        \"module_name\": \"lustre/ui/colour\",
        \"package_name\": \"lustre_ui\",
        \"version\": \"0.4.0\"
    },
    {
        \"name\": \"App\",
        \"documentation\": \"Represents a constructed Lustre application that is ready to be started.\\n Depending on where you want the application to run, you have a few options:\\n\\n - Use [`start`](#start) to start a single-page-application in the browser.\\n\\n   This is the most common way to start a Lustre application. If you're new to\\n   Lustre or frontend development in general, make sure you check out the\\n   [examples](https://github.com/lustre-labs/lustre/tree/main/examples) or the\\n   [quickstart guide]()\\n\\n - Use [`start_server_component`](#start_server_component) to start a Lustre\\n   Server Component anywhere Gleam will run: Erlang, Node, Deno, or in the\\n   browser. If you're running on the BEAM though, you should...\\n\\n - Use [`start_actor`](#start_actor) to start a Lustre Server Component only\\n   for the Erlang target. BEAM users should always prefer this over\\n   `start_server_component` so they can take advantage of OTP features.\\n\\n - Use [`register`](#register) to register a component in the browser to be\\n   used as a Custom Element. This is useful even if you're not using Lustre\\n   to build a SPA.\\n\\n If you're only interested in using Lustre as a HTML templating engine, you\\n don't need an `App` at all! You can render an element directly using the\\n [`element.to_string`](./lustre/element#to_string) function.\",
        \"nature\": \"type_definition\",
        \"metadata\": {
            \"deprecation\": null
        },
        \"json_signature\": {
            \"name\": \"App\",
            \"type\": \"type-definition\",
            \"parameters\": 3,
            \"deprecation\": \" Represents a constructed Lustre application that is ready to be started.\\n Depending on where you want the application to run, you have a few options:\\n\\n - Use [`start`](#start) to start a single-page-application in the browser.\\n\\n   This is the most common way to start a Lustre application. If you're new to\\n   Lustre or frontend development in general, make sure you check out the\\n   [examples](https://github.com/lustre-labs/lustre/tree/main/examples) or the\\n   [quickstart guide]()\\n\\n - Use [`start_server_component`](#start_server_component) to start a Lustre\\n   Server Component anywhere Gleam will run: Erlang, Node, Deno, or in the\\n   browser. If you're running on the BEAM though, you should...\\n\\n - Use [`start_actor`](#start_actor) to start a Lustre Server Component only\\n   for the Erlang target. BEAM users should always prefer this over\\n   `start_server_component` so they can take advantage of OTP features.\\n\\n - Use [`register`](#register) to register a component in the browser to be\\n   used as a Custom Element. This is useful even if you're not using Lustre\\n   to build a SPA.\\n\\n If you're only interested in using Lustre as a HTML templating engine, you\\n don't need an `App` at all! You can render an element directly using the\\n [`element.to_string`](./lustre/element#to_string) function.\\n\",
            \"constructors\": [],
            \"documentation\": \" Represents a constructed Lustre application that is ready to be started.\\n Depending on where you want the application to run, you have a few options:\\n\\n - Use [`start`](#start) to start a single-page-application in the browser.\\n\\n   This is the most common way to start a Lustre application. If you're new to\\n   Lustre or frontend development in general, make sure you check out the\\n   [examples](https://github.com/lustre-labs/lustre/tree/main/examples) or the\\n   [quickstart guide]()\\n\\n - Use [`start_server_component`](#start_server_component) to start a Lustre\\n   Server Component anywhere Gleam will run: Erlang, Node, Deno, or in the\\n   browser. If you're running on the BEAM though, you should...\\n\\n - Use [`start_actor`](#start_actor) to start a Lustre Server Component only\\n   for the Erlang target. BEAM users should always prefer this over\\n   `start_server_component` so they can take advantage of OTP features.\\n\\n - Use [`register`](#register) to register a component in the browser to be\\n   used as a Custom Element. This is useful even if you're not using Lustre\\n   to build a SPA.\\n\\n If you're only interested in using Lustre as a HTML templating engine, you\\n don't need an `App` at all! You can render an element directly using the\\n [`element.to_string`](./lustre/element#to_string) function.\\n\"
        },
        \"module_name\": \"lustre\",
        \"package_name\": \"lustre\",
        \"version\": \"4.1.7\"
    },
    {
        \"name\": \"border_app\",
        \"documentation\": \"\",
        \"nature\": \"function\",
        \"metadata\": {
            \"deprecation\": null,
            \"implementations\": {
                \"gleam\": false,
                \"uses_erlang_externals\": true,
                \"uses_javascript_externals\": true
            }
        },
        \"json_signature\": {
            \"name\": \"border_app\",
            \"type\": \"function\",
            \"return\": {
                \"ref\": \"3.1.4\",
                \"name\": \"Attribute\",
                \"type\": \"named\",
                \"module\": \"lustre/attribute\",
                \"package\": \"lustre\",
                \"parameters\": [
                    {
                        \"id\": 0,
                        \"type\": \"variable\"
                    }
                ]
            },
            \"parameters\": [],
            \"deprecation\": null,
            \"documentation\": null,
            \"implementations\": {
                \"gleam\": false,
                \"uses_erlang_externals\": true,
                \"uses_javascript_externals\": true
            }
        },
        \"module_name\": \"lustre/ui/classes\",
        \"package_name\": \"lustre_ui\",
        \"version\": \"0.3.0\"
    },
    {
        \"name\": \"App\",
        \"documentation\": \"\",
        \"nature\": \"type_definition\",
        \"metadata\": {
            \"deprecation\": null
        },
        \"json_signature\": {
            \"name\": \"App\",
            \"type\": \"type-definition\",
            \"parameters\": 3,
            \"deprecation\": \"\\n\",
            \"constructors\": [],
            \"documentation\": \"\\n\"
        },
        \"module_name\": \"lustre\",
        \"package_name\": \"lustre\",
        \"version\": \"3.1.2\"
    },
    {
        \"name\": \"App\",
        \"documentation\": \"\",
        \"nature\": \"type_definition\",
        \"metadata\": {
            \"deprecation\": null
        },
        \"json_signature\": {
            \"name\": \"App\",
            \"type\": \"type-definition\",
            \"parameters\": 3,
            \"deprecation\": \"\\n\",
            \"constructors\": [],
            \"documentation\": \"\\n\"
        },
        \"module_name\": \"lustre\",
        \"package_name\": \"lustre\",
        \"version\": \"3.1.4\"
    },
    {
        \"name\": \"InlineKeyboardButton\",
        \"documentation\": \"\",
        \"nature\": \"type_definition\",
        \"metadata\": {
            \"deprecation\": null
        },
        \"json_signature\": {
            \"name\": \"InlineKeyboardButton\",
            \"type\": \"type-definition\",
            \"parameters\": 0,
            \"deprecation\": null,
            \"constructors\": [
                {
                    \"name\": \"InlineKeyboardButton\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": {
                                \"ref\": null,
                                \"name\": \"String\",
                                \"type\": \"named\",
                                \"module\": \"gleam\",
                                \"package\": \"\",
                                \"parameters\": []
                            },
                            \"label\": \"text\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"String\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"url\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"String\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"callback_data\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": \"0.1.1\",
                                        \"name\": \"WebAppInfo\",
                                        \"type\": \"named\",
                                        \"module\": \"telega/model\",
                                        \"package\": \"telega\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"web_app\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": \"0.1.1\",
                                        \"name\": \"LoginUrl\",
                                        \"type\": \"named\",
                                        \"module\": \"telega/model\",
                                        \"package\": \"telega\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"login_url\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"String\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"switch_inline_query\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"String\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"switch_inline_query_current_chat\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": \"0.1.1\",
                                        \"name\": \"SwitchInlineQueryChosenChat\",
                                        \"type\": \"named\",
                                        \"module\": \"telega/model\",
                                        \"package\": \"telega\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"switch_inline_query_chosen_chat\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"Bool\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"pay\"
                        }
                    ],
                    \"documentation\": \" **Official reference:** https://core.telegram.org/bots/api#inlinekeyboardbutton\"
                }
            ],
            \"documentation\": null
        },
        \"module_name\": \"telega/model\",
        \"package_name\": \"telega\",
        \"version\": \"0.1.1\"
    },
    {
        \"name\": \"Message\",
        \"documentation\": \"\",
        \"nature\": \"type_definition\",
        \"metadata\": {
            \"deprecation\": null
        },
        \"json_signature\": {
            \"name\": \"Message\",
            \"type\": \"type-definition\",
            \"parameters\": 0,
            \"deprecation\": null,
            \"constructors\": [
                {
                    \"name\": \"Message\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": {
                                \"ref\": null,
                                \"name\": \"Int\",
                                \"type\": \"named\",
                                \"module\": \"gleam\",
                                \"package\": \"\",
                                \"parameters\": []
                            },
                            \"label\": \"message_id\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"Int\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"message_thread_id\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": \"0.1.1\",
                                        \"name\": \"User\",
                                        \"type\": \"named\",
                                        \"module\": \"telega/model\",
                                        \"package\": \"telega\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"from\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": \"0.1.1\",
                                        \"name\": \"Chat\",
                                        \"type\": \"named\",
                                        \"module\": \"telega/model\",
                                        \"package\": \"telega\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"sender_chat\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"Int\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"sender_boost_count\"
                        },
                        {
                            \"type\": {
                                \"ref\": null,
                                \"name\": \"Int\",
                                \"type\": \"named\",
                                \"module\": \"gleam\",
                                \"package\": \"\",
                                \"parameters\": []
                            },
                            \"label\": \"date\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.1.1\",
                                \"name\": \"Chat\",
                                \"type\": \"named\",
                                \"module\": \"telega/model\",
                                \"package\": \"telega\",
                                \"parameters\": []
                            },
                            \"label\": \"chat\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"Bool\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"is_topic_message\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"Bool\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"is_automatic_forward\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": \"0.1.1\",
                                        \"name\": \"Message\",
                                        \"type\": \"named\",
                                        \"module\": \"telega/model\",
                                        \"package\": \"telega\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"reply_to_message\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": \"0.1.1\",
                                        \"name\": \"User\",
                                        \"type\": \"named\",
                                        \"module\": \"telega/model\",
                                        \"package\": \"telega\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"via_bot\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"Int\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"edit_date\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"Bool\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"has_protected_content\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"Bool\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"is_from_offline\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"String\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"media_group_id\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"String\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"author_signature\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"String\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"text\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"List\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": [
                                            {
                                                \"ref\": \"0.1.1\",
                                                \"name\": \"MessageEntity\",
                                                \"type\": \"named\",
                                                \"module\": \"telega/model\",
                                                \"package\": \"telega\",
                                                \"parameters\": []
                                            }
                                        ]
                                    }
                                ]
                            },
                            \"label\": \"entities\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"String\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"caption\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"List\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": [
                                            {
                                                \"ref\": \"0.1.1\",
                                                \"name\": \"MessageEntity\",
                                                \"type\": \"named\",
                                                \"module\": \"telega/model\",
                                                \"package\": \"telega\",
                                                \"parameters\": []
                                            }
                                        ]
                                    }
                                ]
                            },
                            \"label\": \"caption_entities\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"Bool\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"has_media_spoiler\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"List\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": [
                                            {
                                                \"ref\": \"0.1.1\",
                                                \"name\": \"User\",
                                                \"type\": \"named\",
                                                \"module\": \"telega/model\",
                                                \"package\": \"telega\",
                                                \"parameters\": []
                                            }
                                        ]
                                    }
                                ]
                            },
                            \"label\": \"new_chat_members\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": \"0.1.1\",
                                        \"name\": \"User\",
                                        \"type\": \"named\",
                                        \"module\": \"telega/model\",
                                        \"package\": \"telega\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"left_chat_member\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"String\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"new_chat_title\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"Bool\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"delete_chat_photo\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"Bool\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"group_chat_created\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"Bool\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"supergroup_chat_created\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"Bool\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"channel_chat_created\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"Int\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"migrate_to_chat_id\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"Int\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"migrate_from_chat_id\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"String\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"connected_website\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": \"0.1.1\",
                                        \"name\": \"WebAppData\",
                                        \"type\": \"named\",
                                        \"module\": \"telega/model\",
                                        \"package\": \"telega\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"web_app_data\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": \"0.1.1\",
                                        \"name\": \"InlineKeyboardMarkup\",
                                        \"type\": \"named\",
                                        \"module\": \"telega/model\",
                                        \"package\": \"telega\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"reply_markup\"
                        }
                    ],
                    \"documentation\": \" **Official reference:** https://core.telegram.org/bots/api#message\"
                }
            ],
            \"documentation\": null
        },
        \"module_name\": \"telega/model\",
        \"package_name\": \"telega\",
        \"version\": \"0.1.1\"
    },
    {
        \"name\": \"encode_web_app_info\",
        \"documentation\": \"\",
        \"nature\": \"function\",
        \"metadata\": {
            \"deprecation\": null,
            \"implementations\": {
                \"gleam\": false,
                \"uses_erlang_externals\": true,
                \"uses_javascript_externals\": true
            }
        },
        \"json_signature\": {
            \"name\": \"encode_web_app_info\",
            \"type\": \"function\",
            \"return\": {
                \"ref\": \"1.0.1\",
                \"name\": \"Json\",
                \"type\": \"named\",
                \"module\": \"gleam/json\",
                \"package\": \"gleam_json\",
                \"parameters\": []
            },
            \"parameters\": [
                {
                    \"type\": {
                        \"ref\": \"0.1.0\",
                        \"name\": \"WebAppInfo\",
                        \"type\": \"named\",
                        \"module\": \"telega/model\",
                        \"package\": \"telega\",
                        \"parameters\": []
                    },
                    \"label\": null
                }
            ],
            \"deprecation\": null,
            \"documentation\": null,
            \"implementations\": {
                \"gleam\": false,
                \"uses_erlang_externals\": true,
                \"uses_javascript_externals\": true
            }
        },
        \"module_name\": \"telega/model\",
        \"package_name\": \"telega\",
        \"version\": \"0.1.0\"
    },
    {
        \"name\": \"decode_web_app_data\",
        \"documentation\": \"\",
        \"nature\": \"function\",
        \"metadata\": {
            \"deprecation\": null,
            \"implementations\": {
                \"gleam\": false,
                \"uses_erlang_externals\": true,
                \"uses_javascript_externals\": true
            }
        },
        \"json_signature\": {
            \"name\": \"decode_web_app_data\",
            \"type\": \"function\",
            \"return\": {
                \"ref\": null,
                \"name\": \"Result\",
                \"type\": \"named\",
                \"module\": \"gleam\",
                \"package\": \"\",
                \"parameters\": [
                    {
                        \"ref\": \"0.1.1\",
                        \"name\": \"WebAppData\",
                        \"type\": \"named\",
                        \"module\": \"telega/model\",
                        \"package\": \"telega\",
                        \"parameters\": []
                    },
                    {
                        \"ref\": null,
                        \"name\": \"List\",
                        \"type\": \"named\",
                        \"module\": \"gleam\",
                        \"package\": \"\",
                        \"parameters\": [
                            {
                                \"ref\": \"0.37.0\",
                                \"name\": \"DecodeError\",
                                \"type\": \"named\",
                                \"module\": \"gleam/dynamic\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": []
                            }
                        ]
                    }
                ]
            },
            \"parameters\": [
                {
                    \"type\": {
                        \"ref\": \"0.37.0\",
                        \"name\": \"Dynamic\",
                        \"type\": \"named\",
                        \"module\": \"gleam/dynamic\",
                        \"package\": \"gleam_stdlib\",
                        \"parameters\": []
                    },
                    \"label\": null
                }
            ],
            \"deprecation\": null,
            \"documentation\": null,
            \"implementations\": {
                \"gleam\": false,
                \"uses_erlang_externals\": true,
                \"uses_javascript_externals\": true
            }
        },
        \"module_name\": \"telega/model\",
        \"package_name\": \"telega\",
        \"version\": \"0.1.1\"
    },
    {
        \"name\": \"decode_web_app_info\",
        \"documentation\": \"\",
        \"nature\": \"function\",
        \"metadata\": {
            \"deprecation\": null,
            \"implementations\": {
                \"gleam\": false,
                \"uses_erlang_externals\": true,
                \"uses_javascript_externals\": true
            }
        },
        \"json_signature\": {
            \"name\": \"decode_web_app_info\",
            \"type\": \"function\",
            \"return\": {
                \"ref\": null,
                \"name\": \"Result\",
                \"type\": \"named\",
                \"module\": \"gleam\",
                \"package\": \"\",
                \"parameters\": [
                    {
                        \"ref\": \"0.1.1\",
                        \"name\": \"WebAppInfo\",
                        \"type\": \"named\",
                        \"module\": \"telega/model\",
                        \"package\": \"telega\",
                        \"parameters\": []
                    },
                    {
                        \"ref\": null,
                        \"name\": \"List\",
                        \"type\": \"named\",
                        \"module\": \"gleam\",
                        \"package\": \"\",
                        \"parameters\": [
                            {
                                \"ref\": \"0.37.0\",
                                \"name\": \"DecodeError\",
                                \"type\": \"named\",
                                \"module\": \"gleam/dynamic\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": []
                            }
                        ]
                    }
                ]
            },
            \"parameters\": [
                {
                    \"type\": {
                        \"ref\": \"0.37.0\",
                        \"name\": \"Dynamic\",
                        \"type\": \"named\",
                        \"module\": \"gleam/dynamic\",
                        \"package\": \"gleam_stdlib\",
                        \"parameters\": []
                    },
                    \"label\": null
                }
            ],
            \"deprecation\": null,
            \"documentation\": null,
            \"implementations\": {
                \"gleam\": false,
                \"uses_erlang_externals\": true,
                \"uses_javascript_externals\": true
            }
        },
        \"module_name\": \"telega/model\",
        \"package_name\": \"telega\",
        \"version\": \"0.1.1\"
    },
    {
        \"name\": \"decode_web_app_data\",
        \"documentation\": \"\",
        \"nature\": \"function\",
        \"metadata\": {
            \"deprecation\": null,
            \"implementations\": {
                \"gleam\": false,
                \"uses_erlang_externals\": true,
                \"uses_javascript_externals\": true
            }
        },
        \"json_signature\": {
            \"name\": \"decode_web_app_data\",
            \"type\": \"function\",
            \"return\": {
                \"ref\": null,
                \"name\": \"Result\",
                \"type\": \"named\",
                \"module\": \"gleam\",
                \"package\": \"\",
                \"parameters\": [
                    {
                        \"ref\": \"0.1.0\",
                        \"name\": \"WebAppData\",
                        \"type\": \"named\",
                        \"module\": \"telega/model\",
                        \"package\": \"telega\",
                        \"parameters\": []
                    },
                    {
                        \"ref\": null,
                        \"name\": \"List\",
                        \"type\": \"named\",
                        \"module\": \"gleam\",
                        \"package\": \"\",
                        \"parameters\": [
                            {
                                \"ref\": \"0.37.0\",
                                \"name\": \"DecodeError\",
                                \"type\": \"named\",
                                \"module\": \"gleam/dynamic\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": []
                            }
                        ]
                    }
                ]
            },
            \"parameters\": [
                {
                    \"type\": {
                        \"ref\": \"0.37.0\",
                        \"name\": \"Dynamic\",
                        \"type\": \"named\",
                        \"module\": \"gleam/dynamic\",
                        \"package\": \"gleam_stdlib\",
                        \"parameters\": []
                    },
                    \"label\": null
                }
            ],
            \"deprecation\": null,
            \"documentation\": null,
            \"implementations\": {
                \"gleam\": false,
                \"uses_erlang_externals\": true,
                \"uses_javascript_externals\": true
            }
        },
        \"module_name\": \"telega/model\",
        \"package_name\": \"telega\",
        \"version\": \"0.1.0\"
    },
    {
        \"name\": \"Limits\",
        \"documentation\": \"\",
        \"nature\": \"type_definition\",
        \"metadata\": {
            \"deprecation\": null
        },
        \"json_signature\": {
            \"name\": \"Limits\",
            \"type\": \"type-definition\",
            \"parameters\": 0,
            \"deprecation\": null,
            \"constructors\": [
                {
                    \"name\": \"Limits\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": {
                                \"ref\": null,
                                \"name\": \"Int\",
                                \"type\": \"named\",
                                \"module\": \"gleam\",
                                \"package\": \"\",
                                \"parameters\": []
                            },
                            \"label\": \"app_limit\"
                        },
                        {
                            \"type\": {
                                \"ref\": null,
                                \"name\": \"Int\",
                                \"type\": \"named\",
                                \"module\": \"gleam\",
                                \"package\": \"\",
                                \"parameters\": []
                            },
                            \"label\": \"app_remaining\"
                        },
                        {
                            \"type\": {
                                \"ref\": null,
                                \"name\": \"Int\",
                                \"type\": \"named\",
                                \"module\": \"gleam\",
                                \"package\": \"\",
                                \"parameters\": []
                            },
                            \"label\": \"app_reset\"
                        }
                    ],
                    \"documentation\": null
                }
            ],
            \"documentation\": null
        },
        \"module_name\": \"wimp\",
        \"package_name\": \"wimp\",
        \"version\": \"1.1.0\"
    },
    {
        \"name\": \"ReleaseMeta\",
        \"documentation\": \"Meta for a hex release\",
        \"nature\": \"type_definition\",
        \"metadata\": {
            \"deprecation\": null
        },
        \"json_signature\": {
            \"name\": \"ReleaseMeta\",
            \"type\": \"type-definition\",
            \"parameters\": 0,
            \"deprecation\": \" Meta for a hex release\",
            \"constructors\": [
                {
                    \"name\": \"ReleaseMeta\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"String\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"app\"
                        },
                        {
                            \"type\": {
                                \"ref\": null,
                                \"name\": \"List\",
                                \"type\": \"named\",
                                \"module\": \"gleam\",
                                \"package\": \"\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"String\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"build_tools\"
                        }
                    ],
                    \"documentation\": null
                }
            ],
            \"documentation\": \" Meta for a hex release\"
        },
        \"module_name\": \"gleam/hexpm\",
        \"package_name\": \"gleam_hexpm\",
        \"version\": \"1.0.0\"
    },
    {
        \"name\": \"Scale\",
        \"documentation\": \"\",
        \"nature\": \"type_definition\",
        \"metadata\": {
            \"deprecation\": null
        },
        \"json_signature\": {
            \"name\": \"Scale\",
            \"type\": \"type-definition\",
            \"parameters\": 0,
            \"deprecation\": null,
            \"constructors\": [
                {
                    \"name\": \"Scale\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"app_background\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"app_background_subtle\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"app_border\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"element_background\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"element_background_hover\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"element_background_strong\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"element_border_subtle\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"element_border_strong\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"solid_background\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"solid_background_hover\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"text_high_contrast\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"text_low_contrast\"
                        }
                    ],
                    \"documentation\": null
                }
            ],
            \"documentation\": null
        },
        \"module_name\": \"lustre/ui/colour\",
        \"package_name\": \"lustre_ui\",
        \"version\": \"0.2.2\"
    },
    {
        \"name\": \"Scale\",
        \"documentation\": \"\",
        \"nature\": \"type_definition\",
        \"metadata\": {
            \"deprecation\": null
        },
        \"json_signature\": {
            \"name\": \"Scale\",
            \"type\": \"type-definition\",
            \"parameters\": 0,
            \"deprecation\": null,
            \"constructors\": [
                {
                    \"name\": \"Scale\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"app_background\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"app_background_subtle\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"app_border\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"element_background\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"element_background_hover\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"element_background_strong\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"element_border_subtle\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"element_border_strong\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"solid_background\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"solid_background_hover\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"text_high_contrast\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"1.4.0\",
                                \"name\": \"Colour\",
                                \"type\": \"named\",
                                \"module\": \"gleam_community/colour\",
                                \"package\": \"gleam_community_colour\",
                                \"parameters\": []
                            },
                            \"label\": \"text_low_contrast\"
                        }
                    ],
                    \"documentation\": null
                }
            ],
            \"documentation\": null
        },
        \"module_name\": \"lustre/ui/util/colour\",
        \"package_name\": \"lustre_ui\",
        \"version\": \"0.6.0\"
    },
    {
        \"name\": \"gateway_partial_app_decoder\",
        \"documentation\": \"\",
        \"nature\": \"function\",
        \"metadata\": {
            \"deprecation\": null,
            \"implementations\": {
                \"gleam\": false,
                \"uses_erlang_externals\": true,
                \"uses_javascript_externals\": true
            }
        },
        \"json_signature\": {
            \"name\": \"gateway_partial_app_decoder\",
            \"type\": \"function\",
            \"return\": {
                \"type\": \"fn\",
                \"params\": [
                    {
                        \"ref\": \"0.37.0\",
                        \"name\": \"Dynamic\",
                        \"type\": \"named\",
                        \"module\": \"gleam/dynamic\",
                        \"package\": \"gleam_stdlib\",
                        \"parameters\": []
                    }
                ],
                \"return\": {
                    \"ref\": null,
                    \"name\": \"Result\",
                    \"type\": \"named\",
                    \"module\": \"gleam\",
                    \"package\": \"\",
                    \"parameters\": [
                        {
                            \"ref\": \"0.1.0\",
                            \"name\": \"ReadyApplication\",
                            \"type\": \"named\",
                            \"module\": \"glyph/models/discord\",
                            \"package\": \"glyph\",
                            \"parameters\": []
                        },
                        {
                            \"ref\": null,
                            \"name\": \"List\",
                            \"type\": \"named\",
                            \"module\": \"gleam\",
                            \"package\": \"\",
                            \"parameters\": [
                                {
                                    \"ref\": \"0.37.0\",
                                    \"name\": \"DecodeError\",
                                    \"type\": \"named\",
                                    \"module\": \"gleam/dynamic\",
                                    \"package\": \"gleam_stdlib\",
                                    \"parameters\": []
                                }
                            ]
                        }
                    ]
                }
            },
            \"parameters\": [],
            \"deprecation\": null,
            \"documentation\": null,
            \"implementations\": {
                \"gleam\": false,
                \"uses_erlang_externals\": true,
                \"uses_javascript_externals\": true
            }
        },
        \"module_name\": \"glyph/models/decoders\",
        \"package_name\": \"glyph\",
        \"version\": \"0.1.0\"
    },
    {
        \"name\": \"bg_app_subtle\",
        \"documentation\": \"\",
        \"nature\": \"function\",
        \"metadata\": {
            \"deprecation\": null,
            \"implementations\": {
                \"gleam\": false,
                \"uses_erlang_externals\": true,
                \"uses_javascript_externals\": true
            }
        },
        \"json_signature\": {
            \"name\": \"bg_app_subtle\",
            \"type\": \"function\",
            \"return\": {
                \"ref\": \"3.1.4\",
                \"name\": \"Attribute\",
                \"type\": \"named\",
                \"module\": \"lustre/attribute\",
                \"package\": \"lustre\",
                \"parameters\": [
                    {
                        \"id\": 0,
                        \"type\": \"variable\"
                    }
                ]
            },
            \"parameters\": [],
            \"deprecation\": null,
            \"documentation\": null,
            \"implementations\": {
                \"gleam\": false,
                \"uses_erlang_externals\": true,
                \"uses_javascript_externals\": true
            }
        },
        \"module_name\": \"lustre/ui/classes\",
        \"package_name\": \"lustre_ui\",
        \"version\": \"0.2.2\"
    },
    {
        \"name\": \"App\",
        \"documentation\": \"Represents a constructed Lustre application that is ready to be started.\\n Depending on where you want the application to run, you have a few options:\\n\\n - Use [`start`](#start) to start a single-page-application in the browser.\\n\\n   This is the most common way to start a Lustre application. If you're new to\\n   Lustre or frontend development in general, make sure you check out the\\n   [examples](https://github.com/lustre-labs/lustre/tree/main/examples) or the\\n   [quickstart guide]()\\n\\n - Use [`start_server_component`](#start_server_component) to start a Lustre\\n   Server Component anywhere Gleam will run: Erlang, Node, Deno, or in the\\n   browser. If you're running on the BEAM though, you should...\\n\\n - Use [`start_actor`](#start_actor) to start a Lustre Server Component only\\n   for the Erlang target. BEAM users should always prefer this over\\n   `start_server_component` so they can take advantage of OTP features.\\n\\n - Use [`register`](#register) to register a component in the browser to be\\n   used as a Custom Element. This is useful even if you're not using Lustre\\n   to build a SPA.\\n\\n If you're only interested in using Lustre as a HTML templating engine, you\\n don't need an `App` at all! You can render an element directly using the\\n [`element.to_string`](./lustre/element#to_string) function.\",
        \"nature\": \"type_definition\",
        \"metadata\": {
            \"deprecation\": null
        },
        \"json_signature\": {
            \"name\": \"App\",
            \"type\": \"type-definition\",
            \"parameters\": 3,
            \"deprecation\": \" Represents a constructed Lustre application that is ready to be started.\\n Depending on where you want the application to run, you have a few options:\\n\\n - Use [`start`](#start) to start a single-page-application in the browser.\\n\\n   This is the most common way to start a Lustre application. If you're new to\\n   Lustre or frontend development in general, make sure you check out the\\n   [examples](https://github.com/lustre-labs/lustre/tree/main/examples) or the\\n   [quickstart guide]()\\n\\n - Use [`start_server_component`](#start_server_component) to start a Lustre\\n   Server Component anywhere Gleam will run: Erlang, Node, Deno, or in the\\n   browser. If you're running on the BEAM though, you should...\\n\\n - Use [`start_actor`](#start_actor) to start a Lustre Server Component only\\n   for the Erlang target. BEAM users should always prefer this over\\n   `start_server_component` so they can take advantage of OTP features.\\n\\n - Use [`register`](#register) to register a component in the browser to be\\n   used as a Custom Element. This is useful even if you're not using Lustre\\n   to build a SPA.\\n\\n If you're only interested in using Lustre as a HTML templating engine, you\\n don't need an `App` at all! You can render an element directly using the\\n [`element.to_string`](./lustre/element#to_string) function.\\n\",
            \"constructors\": [],
            \"documentation\": \" Represents a constructed Lustre application that is ready to be started.\\n Depending on where you want the application to run, you have a few options:\\n\\n - Use [`start`](#start) to start a single-page-application in the browser.\\n\\n   This is the most common way to start a Lustre application. If you're new to\\n   Lustre or frontend development in general, make sure you check out the\\n   [examples](https://github.com/lustre-labs/lustre/tree/main/examples) or the\\n   [quickstart guide]()\\n\\n - Use [`start_server_component`](#start_server_component) to start a Lustre\\n   Server Component anywhere Gleam will run: Erlang, Node, Deno, or in the\\n   browser. If you're running on the BEAM though, you should...\\n\\n - Use [`start_actor`](#start_actor) to start a Lustre Server Component only\\n   for the Erlang target. BEAM users should always prefer this over\\n   `start_server_component` so they can take advantage of OTP features.\\n\\n - Use [`register`](#register) to register a component in the browser to be\\n   used as a Custom Element. This is useful even if you're not using Lustre\\n   to build a SPA.\\n\\n If you're only interested in using Lustre as a HTML templating engine, you\\n don't need an `App` at all! You can render an element directly using the\\n [`element.to_string`](./lustre/element#to_string) function.\\n\"
        },
        \"module_name\": \"lustre\",
        \"package_name\": \"lustre\",
        \"version\": \"4.1.4\"
    },
    {
        \"name\": \"App\",
        \"documentation\": \"Represents a constructed Lustre application that is ready to be started.\\n Depending on where you want the application to run, you have a few options:\\n\\n - Use [`start`](#start) to start a single-page-application in the browser.\\n\\n   This is the most common way to start a Lustre application. If you're new to\\n   Lustre or frontend development in general, make sure you check out the\\n   [examples](https://github.com/lustre-labs/lustre/tree/main/examples) or the\\n   [quickstart guide]()\\n\\n - Use [`start_server_component`](#start_server_component) to start a Lustre\\n   Server Component anywhere Gleam will run: Erlang, Node, Deno, or in the\\n   browser. If you're running on the BEAM though, you should...\\n\\n - Use [`start_actor`](#start_actor) to start a Lustre Server Component only\\n   for the Erlang target. BEAM users should always prefer this over\\n   `start_server_component` so they can take advantage of OTP features.\\n\\n - Use [`register`](#register) to register a component in the browser to be\\n   used as a Custom Element. This is useful even if you're not using Lustre\\n   to build a SPA.\\n\\n If you're only interested in using Lustre as a HTML templating engine, you\\n don't need an `App` at all! You can render an element directly using the\\n [`element.to_string`](./lustre/element.html#to_string) function.\",
        \"nature\": \"type_definition\",
        \"metadata\": {
            \"deprecation\": null
        },
        \"json_signature\": {
            \"name\": \"App\",
            \"type\": \"type-definition\",
            \"parameters\": 3,
            \"deprecation\": \" Represents a constructed Lustre application that is ready to be started.\\n Depending on where you want the application to run, you have a few options:\\n\\n - Use [`start`](#start) to start a single-page-application in the browser.\\n\\n   This is the most common way to start a Lustre application. If you're new to\\n   Lustre or frontend development in general, make sure you check out the\\n   [examples](https://github.com/lustre-labs/lustre/tree/main/examples) or the\\n   [quickstart guide]()\\n\\n - Use [`start_server_component`](#start_server_component) to start a Lustre\\n   Server Component anywhere Gleam will run: Erlang, Node, Deno, or in the\\n   browser. If you're running on the BEAM though, you should...\\n\\n - Use [`start_actor`](#start_actor) to start a Lustre Server Component only\\n   for the Erlang target. BEAM users should always prefer this over\\n   `start_server_component` so they can take advantage of OTP features.\\n\\n - Use [`register`](#register) to register a component in the browser to be\\n   used as a Custom Element. This is useful even if you're not using Lustre\\n   to build a SPA.\\n\\n If you're only interested in using Lustre as a HTML templating engine, you\\n don't need an `App` at all! You can render an element directly using the\\n [`element.to_string`](./lustre/element.html#to_string) function.\\n\",
            \"constructors\": [],
            \"documentation\": \" Represents a constructed Lustre application that is ready to be started.\\n Depending on where you want the application to run, you have a few options:\\n\\n - Use [`start`](#start) to start a single-page-application in the browser.\\n\\n   This is the most common way to start a Lustre application. If you're new to\\n   Lustre or frontend development in general, make sure you check out the\\n   [examples](https://github.com/lustre-labs/lustre/tree/main/examples) or the\\n   [quickstart guide]()\\n\\n - Use [`start_server_component`](#start_server_component) to start a Lustre\\n   Server Component anywhere Gleam will run: Erlang, Node, Deno, or in the\\n   browser. If you're running on the BEAM though, you should...\\n\\n - Use [`start_actor`](#start_actor) to start a Lustre Server Component only\\n   for the Erlang target. BEAM users should always prefer this over\\n   `start_server_component` so they can take advantage of OTP features.\\n\\n - Use [`register`](#register) to register a component in the browser to be\\n   used as a Custom Element. This is useful even if you're not using Lustre\\n   to build a SPA.\\n\\n If you're only interested in using Lustre as a HTML templating engine, you\\n don't need an `App` at all! You can render an element directly using the\\n [`element.to_string`](./lustre/element.html#to_string) function.\\n\"
        },
        \"module_name\": \"lustre\",
        \"package_name\": \"lustre\",
        \"version\": \"4.1.8\"
    },
    {
        \"name\": \"App\",
        \"documentation\": \"Represents a constructed Lustre application that is ready to be started.\\n Depending on where you want the application to run, you have a few options:\\n\\n - Use [`start`](#start) to start a single-page-application in the browser.\\n\\n   This is the most common way to start a Lustre application. If you're new to\\n   Lustre or frontend development in general, make sure you check out the\\n   [examples](https://github.com/lustre-labs/lustre/tree/main/examples) or the\\n   [quickstart guide]()\\n\\n - Use [`start_server_component`](#start_server_component) to start a Lustre\\n   Server Component anywhere Gleam will run: Erlang, Node, Deno, or in the\\n   browser. If you're running on the BEAM though, you should...\\n\\n - Use [`start_actor`](#start_actor) to start a Lustre Server Component only\\n   for the Erlang target. BEAM users should always prefer this over\\n   `start_server_component` so they can take advantage of OTP features.\\n\\n - Use [`register`](#register) to register a component in the browser to be\\n   used as a Custom Element. This is useful even if you're not using Lustre\\n   to build a SPA.\\n\\n If you're only interested in using Lustre as a HTML templating engine, you\\n don't need an `App` at all! You can render an element directly using the\\n [`element.to_string`](./lustre/element#to_string) function.\",
        \"nature\": \"type_definition\",
        \"metadata\": {
            \"deprecation\": null
        },
        \"json_signature\": {
            \"name\": \"App\",
            \"type\": \"type-definition\",
            \"parameters\": 3,
            \"deprecation\": \" Represents a constructed Lustre application that is ready to be started.\\n Depending on where you want the application to run, you have a few options:\\n\\n - Use [`start`](#start) to start a single-page-application in the browser.\\n\\n   This is the most common way to start a Lustre application. If you're new to\\n   Lustre or frontend development in general, make sure you check out the\\n   [examples](https://github.com/lustre-labs/lustre/tree/main/examples) or the\\n   [quickstart guide]()\\n\\n - Use [`start_server_component`](#start_server_component) to start a Lustre\\n   Server Component anywhere Gleam will run: Erlang, Node, Deno, or in the\\n   browser. If you're running on the BEAM though, you should...\\n\\n - Use [`start_actor`](#start_actor) to start a Lustre Server Component only\\n   for the Erlang target. BEAM users should always prefer this over\\n   `start_server_component` so they can take advantage of OTP features.\\n\\n - Use [`register`](#register) to register a component in the browser to be\\n   used as a Custom Element. This is useful even if you're not using Lustre\\n   to build a SPA.\\n\\n If you're only interested in using Lustre as a HTML templating engine, you\\n don't need an `App` at all! You can render an element directly using the\\n [`element.to_string`](./lustre/element#to_string) function.\\n\",
            \"constructors\": [],
            \"documentation\": \" Represents a constructed Lustre application that is ready to be started.\\n Depending on where you want the application to run, you have a few options:\\n\\n - Use [`start`](#start) to start a single-page-application in the browser.\\n\\n   This is the most common way to start a Lustre application. If you're new to\\n   Lustre or frontend development in general, make sure you check out the\\n   [examples](https://github.com/lustre-labs/lustre/tree/main/examples) or the\\n   [quickstart guide]()\\n\\n - Use [`start_server_component`](#start_server_component) to start a Lustre\\n   Server Component anywhere Gleam will run: Erlang, Node, Deno, or in the\\n   browser. If you're running on the BEAM though, you should...\\n\\n - Use [`start_actor`](#start_actor) to start a Lustre Server Component only\\n   for the Erlang target. BEAM users should always prefer this over\\n   `start_server_component` so they can take advantage of OTP features.\\n\\n - Use [`register`](#register) to register a component in the browser to be\\n   used as a Custom Element. This is useful even if you're not using Lustre\\n   to build a SPA.\\n\\n If you're only interested in using Lustre as a HTML templating engine, you\\n don't need an `App` at all! You can render an element directly using the\\n [`element.to_string`](./lustre/element#to_string) function.\\n\"
        },
        \"module_name\": \"lustre\",
        \"package_name\": \"lustre\",
        \"version\": \"4.1.5\"
    },
    {
        \"name\": \"encode_web_app_info\",
        \"documentation\": \"\",
        \"nature\": \"function\",
        \"metadata\": {
            \"deprecation\": null,
            \"implementations\": {
                \"gleam\": false,
                \"uses_erlang_externals\": true,
                \"uses_javascript_externals\": true
            }
        },
        \"json_signature\": {
            \"name\": \"encode_web_app_info\",
            \"type\": \"function\",
            \"return\": {
                \"ref\": \"1.0.1\",
                \"name\": \"Json\",
                \"type\": \"named\",
                \"module\": \"gleam/json\",
                \"package\": \"gleam_json\",
                \"parameters\": []
            },
            \"parameters\": [
                {
                    \"type\": {
                        \"ref\": \"0.1.1\",
                        \"name\": \"WebAppInfo\",
                        \"type\": \"named\",
                        \"module\": \"telega/model\",
                        \"package\": \"telega\",
                        \"parameters\": []
                    },
                    \"label\": null
                }
            ],
            \"deprecation\": null,
            \"documentation\": null,
            \"implementations\": {
                \"gleam\": false,
                \"uses_erlang_externals\": true,
                \"uses_javascript_externals\": true
            }
        },
        \"module_name\": \"telega/model\",
        \"package_name\": \"telega\",
        \"version\": \"0.1.1\"
    },
    {
        \"name\": \"InlineKeyboardButton\",
        \"documentation\": \"\",
        \"nature\": \"type_definition\",
        \"metadata\": {
            \"deprecation\": null
        },
        \"json_signature\": {
            \"name\": \"InlineKeyboardButton\",
            \"type\": \"type-definition\",
            \"parameters\": 0,
            \"deprecation\": null,
            \"constructors\": [
                {
                    \"name\": \"InlineKeyboardButton\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": {
                                \"ref\": null,
                                \"name\": \"String\",
                                \"type\": \"named\",
                                \"module\": \"gleam\",
                                \"package\": \"\",
                                \"parameters\": []
                            },
                            \"label\": \"text\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"String\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"url\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"String\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"callback_data\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": \"0.1.0\",
                                        \"name\": \"WebAppInfo\",
                                        \"type\": \"named\",
                                        \"module\": \"telega/model\",
                                        \"package\": \"telega\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"web_app\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": \"0.1.0\",
                                        \"name\": \"LoginUrl\",
                                        \"type\": \"named\",
                                        \"module\": \"telega/model\",
                                        \"package\": \"telega\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"login_url\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"String\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"switch_inline_query\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"String\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"switch_inline_query_current_chat\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": \"0.1.0\",
                                        \"name\": \"SwitchInlineQueryChosenChat\",
                                        \"type\": \"named\",
                                        \"module\": \"telega/model\",
                                        \"package\": \"telega\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"switch_inline_query_chosen_chat\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"Bool\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"pay\"
                        }
                    ],
                    \"documentation\": \" **Official reference:** https://core.telegram.org/bots/api#inlinekeyboardbutton\"
                }
            ],
            \"documentation\": null
        },
        \"module_name\": \"telega/model\",
        \"package_name\": \"telega\",
        \"version\": \"0.1.0\"
    },
    {
        \"name\": \"decode_web_app_info\",
        \"documentation\": \"\",
        \"nature\": \"function\",
        \"metadata\": {
            \"deprecation\": null,
            \"implementations\": {
                \"gleam\": false,
                \"uses_erlang_externals\": true,
                \"uses_javascript_externals\": true
            }
        },
        \"json_signature\": {
            \"name\": \"decode_web_app_info\",
            \"type\": \"function\",
            \"return\": {
                \"ref\": null,
                \"name\": \"Result\",
                \"type\": \"named\",
                \"module\": \"gleam\",
                \"package\": \"\",
                \"parameters\": [
                    {
                        \"ref\": \"0.1.0\",
                        \"name\": \"WebAppInfo\",
                        \"type\": \"named\",
                        \"module\": \"telega/model\",
                        \"package\": \"telega\",
                        \"parameters\": []
                    },
                    {
                        \"ref\": null,
                        \"name\": \"List\",
                        \"type\": \"named\",
                        \"module\": \"gleam\",
                        \"package\": \"\",
                        \"parameters\": [
                            {
                                \"ref\": \"0.37.0\",
                                \"name\": \"DecodeError\",
                                \"type\": \"named\",
                                \"module\": \"gleam/dynamic\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": []
                            }
                        ]
                    }
                ]
            },
            \"parameters\": [
                {
                    \"type\": {
                        \"ref\": \"0.37.0\",
                        \"name\": \"Dynamic\",
                        \"type\": \"named\",
                        \"module\": \"gleam/dynamic\",
                        \"package\": \"gleam_stdlib\",
                        \"parameters\": []
                    },
                    \"label\": null
                }
            ],
            \"deprecation\": null,
            \"documentation\": null,
            \"implementations\": {
                \"gleam\": false,
                \"uses_erlang_externals\": true,
                \"uses_javascript_externals\": true
            }
        },
        \"module_name\": \"telega/model\",
        \"package_name\": \"telega\",
        \"version\": \"0.1.0\"
    },
    {
        \"name\": \"App\",
        \"documentation\": \"Represents a constructed Lustre application that is ready to be started.\\n Depending on where you want the application to run, you have a few options:\\n\\n - Use [`start`](#start) to start a single-page-application in the browser.\\n\\n   This is the most common way to start a Lustre application. If you're new to\\n   Lustre or frontend development in general, make sure you check out the\\n   [examples](https://github.com/lustre-labs/lustre/tree/main/examples) or the\\n   [quickstart guide]()\\n\\n - Use [`start_server_component`](#start_server_component) to start a Lustre\\n   Server Component anywhere Gleam will run: Erlang, Node, Deno, or in the\\n   browser. If you're running on the BEAM though, you should...\\n\\n - Use [`start_actor`](#start_actor) to start a Lustre Server Component only\\n   for the Erlang target. BEAM users should always prefer this over\\n   `start_server_component` so they can take advantage of OTP features.\\n\\n - Use [`register`](#register) to register a component in the browser to be\\n   used as a Custom Element. This is useful even if you're not using Lustre\\n   to build a SPA.\\n\\n If you're only interested in using Lustre as a HTML templating engine, you\\n don't need an `App` at all! You can render an element directly using the\\n [`element.to_string`](./lustre/element#to_string) function.\",
        \"nature\": \"type_definition\",
        \"metadata\": {
            \"deprecation\": null
        },
        \"json_signature\": {
            \"name\": \"App\",
            \"type\": \"type-definition\",
            \"parameters\": 3,
            \"deprecation\": \" Represents a constructed Lustre application that is ready to be started.\\n Depending on where you want the application to run, you have a few options:\\n\\n - Use [`start`](#start) to start a single-page-application in the browser.\\n\\n   This is the most common way to start a Lustre application. If you're new to\\n   Lustre or frontend development in general, make sure you check out the\\n   [examples](https://github.com/lustre-labs/lustre/tree/main/examples) or the\\n   [quickstart guide]()\\n\\n - Use [`start_server_component`](#start_server_component) to start a Lustre\\n   Server Component anywhere Gleam will run: Erlang, Node, Deno, or in the\\n   browser. If you're running on the BEAM though, you should...\\n\\n - Use [`start_actor`](#start_actor) to start a Lustre Server Component only\\n   for the Erlang target. BEAM users should always prefer this over\\n   `start_server_component` so they can take advantage of OTP features.\\n\\n - Use [`register`](#register) to register a component in the browser to be\\n   used as a Custom Element. This is useful even if you're not using Lustre\\n   to build a SPA.\\n\\n If you're only interested in using Lustre as a HTML templating engine, you\\n don't need an `App` at all! You can render an element directly using the\\n [`element.to_string`](./lustre/element#to_string) function.\\n\",
            \"constructors\": [],
            \"documentation\": \" Represents a constructed Lustre application that is ready to be started.\\n Depending on where you want the application to run, you have a few options:\\n\\n - Use [`start`](#start) to start a single-page-application in the browser.\\n\\n   This is the most common way to start a Lustre application. If you're new to\\n   Lustre or frontend development in general, make sure you check out the\\n   [examples](https://github.com/lustre-labs/lustre/tree/main/examples) or the\\n   [quickstart guide]()\\n\\n - Use [`start_server_component`](#start_server_component) to start a Lustre\\n   Server Component anywhere Gleam will run: Erlang, Node, Deno, or in the\\n   browser. If you're running on the BEAM though, you should...\\n\\n - Use [`start_actor`](#start_actor) to start a Lustre Server Component only\\n   for the Erlang target. BEAM users should always prefer this over\\n   `start_server_component` so they can take advantage of OTP features.\\n\\n - Use [`register`](#register) to register a component in the browser to be\\n   used as a Custom Element. This is useful even if you're not using Lustre\\n   to build a SPA.\\n\\n If you're only interested in using Lustre as a HTML templating engine, you\\n don't need an `App` at all! You can render an element directly using the\\n [`element.to_string`](./lustre/element#to_string) function.\\n\"
        },
        \"module_name\": \"lustre\",
        \"package_name\": \"lustre\",
        \"version\": \"4.1.0\"
    },
    {
        \"name\": \"App\",
        \"documentation\": \"Represents a constructed Lustre application that is ready to be started.\\n Depending on where you want the application to run, you have a few options:\\n\\n - Use [`start`](#start) to start a single-page-application in the browser.\\n\\n   This is the most common way to start a Lustre application. If you're new to\\n   Lustre or frontend development in general, make sure you check out the\\n   [examples](https://github.com/lustre-labs/lustre/tree/main/examples) or the\\n   [quickstart guide]()\\n\\n - Use [`start_server_component`](#start_server_component) to start a Lustre\\n   Server Component anywhere Gleam will run: Erlang, Node, Deno, or in the\\n   browser. If you're running on the BEAM though, you should...\\n\\n - Use [`start_actor`](#start_actor) to start a Lustre Server Component only\\n   for the Erlang target. BEAM users should always prefer this over\\n   `start_server_component` so they can take advantage of OTP features.\\n\\n - Use [`register`](#register) to register a component in the browser to be\\n   used as a Custom Element. This is useful even if you're not using Lustre\\n   to build a SPA.\\n\\n If you're only interested in using Lustre as a HTML templating engine, you\\n don't need an `App` at all! You can render an element directly using the\\n [`element.to_string`](./lustre/element#to_string) function.\",
        \"nature\": \"type_definition\",
        \"metadata\": {
            \"deprecation\": null
        },
        \"json_signature\": {
            \"name\": \"App\",
            \"type\": \"type-definition\",
            \"parameters\": 3,
            \"deprecation\": \" Represents a constructed Lustre application that is ready to be started.\\n Depending on where you want the application to run, you have a few options:\\n\\n - Use [`start`](#start) to start a single-page-application in the browser.\\n\\n   This is the most common way to start a Lustre application. If you're new to\\n   Lustre or frontend development in general, make sure you check out the\\n   [examples](https://github.com/lustre-labs/lustre/tree/main/examples) or the\\n   [quickstart guide]()\\n\\n - Use [`start_server_component`](#start_server_component) to start a Lustre\\n   Server Component anywhere Gleam will run: Erlang, Node, Deno, or in the\\n   browser. If you're running on the BEAM though, you should...\\n\\n - Use [`start_actor`](#start_actor) to start a Lustre Server Component only\\n   for the Erlang target. BEAM users should always prefer this over\\n   `start_server_component` so they can take advantage of OTP features.\\n\\n - Use [`register`](#register) to register a component in the browser to be\\n   used as a Custom Element. This is useful even if you're not using Lustre\\n   to build a SPA.\\n\\n If you're only interested in using Lustre as a HTML templating engine, you\\n don't need an `App` at all! You can render an element directly using the\\n [`element.to_string`](./lustre/element#to_string) function.\\n\",
            \"constructors\": [],
            \"documentation\": \" Represents a constructed Lustre application that is ready to be started.\\n Depending on where you want the application to run, you have a few options:\\n\\n - Use [`start`](#start) to start a single-page-application in the browser.\\n\\n   This is the most common way to start a Lustre application. If you're new to\\n   Lustre or frontend development in general, make sure you check out the\\n   [examples](https://github.com/lustre-labs/lustre/tree/main/examples) or the\\n   [quickstart guide]()\\n\\n - Use [`start_server_component`](#start_server_component) to start a Lustre\\n   Server Component anywhere Gleam will run: Erlang, Node, Deno, or in the\\n   browser. If you're running on the BEAM though, you should...\\n\\n - Use [`start_actor`](#start_actor) to start a Lustre Server Component only\\n   for the Erlang target. BEAM users should always prefer this over\\n   `start_server_component` so they can take advantage of OTP features.\\n\\n - Use [`register`](#register) to register a component in the browser to be\\n   used as a Custom Element. This is useful even if you're not using Lustre\\n   to build a SPA.\\n\\n If you're only interested in using Lustre as a HTML templating engine, you\\n don't need an `App` at all! You can render an element directly using the\\n [`element.to_string`](./lustre/element#to_string) function.\\n\"
        },
        \"module_name\": \"lustre\",
        \"package_name\": \"lustre\",
        \"version\": \"4.1.2\"
    },
    {
        \"name\": \"KeyboardButton\",
        \"documentation\": \"\",
        \"nature\": \"type_definition\",
        \"metadata\": {
            \"deprecation\": null
        },
        \"json_signature\": {
            \"name\": \"KeyboardButton\",
            \"type\": \"type-definition\",
            \"parameters\": 0,
            \"deprecation\": null,
            \"constructors\": [
                {
                    \"name\": \"KeyboardButton\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": {
                                \"ref\": null,
                                \"name\": \"String\",
                                \"type\": \"named\",
                                \"module\": \"gleam\",
                                \"package\": \"\",
                                \"parameters\": []
                            },
                            \"label\": \"text\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": \"0.1.0\",
                                        \"name\": \"KeyboardButtonRequestUsers\",
                                        \"type\": \"named\",
                                        \"module\": \"telega/model\",
                                        \"package\": \"telega\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"request_users\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": \"0.1.0\",
                                        \"name\": \"KeyboardButtonRequestChat\",
                                        \"type\": \"named\",
                                        \"module\": \"telega/model\",
                                        \"package\": \"telega\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"request_chat\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"Bool\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"request_contact\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"Bool\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"request_location\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": \"0.1.0\",
                                        \"name\": \"KeyboardButtonPollType\",
                                        \"type\": \"named\",
                                        \"module\": \"telega/model\",
                                        \"package\": \"telega\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"request_poll\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": \"0.1.0\",
                                        \"name\": \"WebAppInfo\",
                                        \"type\": \"named\",
                                        \"module\": \"telega/model\",
                                        \"package\": \"telega\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"web_app\"
                        }
                    ],
                    \"documentation\": \" This object represents one button of the reply keyboard. For simple text buttons, String can be used instead of this object to specify the button text. The optional fields _web_app_, _request_users_, _request_chat_, _request_contact_, _request_location_, and _request_poll_ are mutually exclusive.\\n\\n **Official reference:** https://core.telegram.org/bots/api#keyboardbutton\"
                }
            ],
            \"documentation\": null
        },
        \"module_name\": \"telega/model\",
        \"package_name\": \"telega\",
        \"version\": \"0.1.0\"
    },
    {
        \"name\": \"Message\",
        \"documentation\": \"\",
        \"nature\": \"type_definition\",
        \"metadata\": {
            \"deprecation\": null
        },
        \"json_signature\": {
            \"name\": \"Message\",
            \"type\": \"type-definition\",
            \"parameters\": 0,
            \"deprecation\": null,
            \"constructors\": [
                {
                    \"name\": \"Message\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": {
                                \"ref\": null,
                                \"name\": \"Int\",
                                \"type\": \"named\",
                                \"module\": \"gleam\",
                                \"package\": \"\",
                                \"parameters\": []
                            },
                            \"label\": \"message_id\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"Int\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"message_thread_id\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": \"0.1.0\",
                                        \"name\": \"User\",
                                        \"type\": \"named\",
                                        \"module\": \"telega/model\",
                                        \"package\": \"telega\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"from\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": \"0.1.0\",
                                        \"name\": \"Chat\",
                                        \"type\": \"named\",
                                        \"module\": \"telega/model\",
                                        \"package\": \"telega\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"sender_chat\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"Int\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"sender_boost_count\"
                        },
                        {
                            \"type\": {
                                \"ref\": null,
                                \"name\": \"Int\",
                                \"type\": \"named\",
                                \"module\": \"gleam\",
                                \"package\": \"\",
                                \"parameters\": []
                            },
                            \"label\": \"date\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.1.0\",
                                \"name\": \"Chat\",
                                \"type\": \"named\",
                                \"module\": \"telega/model\",
                                \"package\": \"telega\",
                                \"parameters\": []
                            },
                            \"label\": \"chat\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"Bool\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"is_topic_message\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"Bool\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"is_automatic_forward\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": \"0.1.0\",
                                        \"name\": \"Message\",
                                        \"type\": \"named\",
                                        \"module\": \"telega/model\",
                                        \"package\": \"telega\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"reply_to_message\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": \"0.1.0\",
                                        \"name\": \"User\",
                                        \"type\": \"named\",
                                        \"module\": \"telega/model\",
                                        \"package\": \"telega\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"via_bot\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"Int\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"edit_date\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"Bool\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"has_protected_content\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"Bool\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"is_from_offline\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"String\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"media_group_id\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"String\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"author_signature\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"String\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"text\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"List\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": [
                                            {
                                                \"ref\": \"0.1.0\",
                                                \"name\": \"MessageEntity\",
                                                \"type\": \"named\",
                                                \"module\": \"telega/model\",
                                                \"package\": \"telega\",
                                                \"parameters\": []
                                            }
                                        ]
                                    }
                                ]
                            },
                            \"label\": \"entities\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"String\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"caption\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"List\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": [
                                            {
                                                \"ref\": \"0.1.0\",
                                                \"name\": \"MessageEntity\",
                                                \"type\": \"named\",
                                                \"module\": \"telega/model\",
                                                \"package\": \"telega\",
                                                \"parameters\": []
                                            }
                                        ]
                                    }
                                ]
                            },
                            \"label\": \"caption_entities\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"Bool\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"has_media_spoiler\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"List\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": [
                                            {
                                                \"ref\": \"0.1.0\",
                                                \"name\": \"User\",
                                                \"type\": \"named\",
                                                \"module\": \"telega/model\",
                                                \"package\": \"telega\",
                                                \"parameters\": []
                                            }
                                        ]
                                    }
                                ]
                            },
                            \"label\": \"new_chat_members\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": \"0.1.0\",
                                        \"name\": \"User\",
                                        \"type\": \"named\",
                                        \"module\": \"telega/model\",
                                        \"package\": \"telega\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"left_chat_member\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"String\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"new_chat_title\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"Bool\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"delete_chat_photo\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"Bool\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"group_chat_created\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"Bool\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"supergroup_chat_created\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"Bool\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"channel_chat_created\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"Int\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"migrate_to_chat_id\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"Int\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"migrate_from_chat_id\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": null,
                                        \"name\": \"String\",
                                        \"type\": \"named\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"connected_website\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": \"0.1.0\",
                                        \"name\": \"WebAppData\",
                                        \"type\": \"named\",
                                        \"module\": \"telega/model\",
                                        \"package\": \"telega\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"web_app_data\"
                        },
                        {
                            \"type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": \"0.1.0\",
                                        \"name\": \"InlineKeyboardMarkup\",
                                        \"type\": \"named\",
                                        \"module\": \"telega/model\",
                                        \"package\": \"telega\",
                                        \"parameters\": []
                                    }
                                ]
                            },
                            \"label\": \"reply_markup\"
                        }
                    ],
                    \"documentation\": \" **Official reference:** https://core.telegram.org/bots/api#message\"
                }
            ],
            \"documentation\": null
        },
        \"module_name\": \"telega/model\",
        \"package_name\": \"telega\",
        \"version\": \"0.1.0\"
    },
    {
        \"name\": \"WebAppData\",
        \"documentation\": \"\",
        \"nature\": \"type_definition\",
        \"metadata\": {
            \"deprecation\": null
        },
        \"json_signature\": {
            \"name\": \"WebAppData\",
            \"type\": \"type-definition\",
            \"parameters\": 0,
            \"deprecation\": null,
            \"constructors\": [
                {
                    \"name\": \"WebAppData\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": {
                                \"ref\": null,
                                \"name\": \"String\",
                                \"type\": \"named\",
                                \"module\": \"gleam\",
                                \"package\": \"\",
                                \"parameters\": []
                            },
                            \"label\": \"data\"
                        },
                        {
                            \"type\": {
                                \"ref\": null,
                                \"name\": \"String\",
                                \"type\": \"named\",
                                \"module\": \"gleam\",
                                \"package\": \"\",
                                \"parameters\": []
                            },
                            \"label\": \"button_text\"
                        }
                    ],
                    \"documentation\": \" Describes data sent from a [Web App](https://core.telegram.org/bots/webapps) to the bot.\"
                }
            ],
            \"documentation\": null
        },
        \"module_name\": \"telega/model\",
        \"package_name\": \"telega\",
        \"version\": \"0.1.1\"
    },
    {
        \"name\": \"WebAppInfo\",
        \"documentation\": \"\",
        \"nature\": \"type_definition\",
        \"metadata\": {
            \"deprecation\": null
        },
        \"json_signature\": {
            \"name\": \"WebAppInfo\",
            \"type\": \"type-definition\",
            \"parameters\": 0,
            \"deprecation\": null,
            \"constructors\": [
                {
                    \"name\": \"WebAppInfo\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": {
                                \"ref\": null,
                                \"name\": \"String\",
                                \"type\": \"named\",
                                \"module\": \"gleam\",
                                \"package\": \"\",
                                \"parameters\": []
                            },
                            \"label\": \"url\"
                        }
                    ],
                    \"documentation\": \" Describes a [Web App](https://core.telegram.org/bots/webapps).\\n\\n **Official reference:** [WebAppInfo](https://core.telegram.org/bots/api#webappinfo)\"
                }
            ],
            \"documentation\": null
        },
        \"module_name\": \"telega/model\",
        \"package_name\": \"telega\",
        \"version\": \"0.1.1\"
    },
    {
        \"name\": \"WebAppInfo\",
        \"documentation\": \"\",
        \"nature\": \"type_definition\",
        \"metadata\": {
            \"deprecation\": null
        },
        \"json_signature\": {
            \"name\": \"WebAppInfo\",
            \"type\": \"type-definition\",
            \"parameters\": 0,
            \"deprecation\": null,
            \"constructors\": [
                {
                    \"name\": \"WebAppInfo\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": {
                                \"ref\": null,
                                \"name\": \"String\",
                                \"type\": \"named\",
                                \"module\": \"gleam\",
                                \"package\": \"\",
                                \"parameters\": []
                            },
                            \"label\": \"url\"
                        }
                    ],
                    \"documentation\": \" Describes a [Web App](https://core.telegram.org/bots/webapps).\\n\\n **Official reference:** [WebAppInfo](https://core.telegram.org/bots/api#webappinfo)\"
                }
            ],
            \"documentation\": null
        },
        \"module_name\": \"telega/model\",
        \"package_name\": \"telega\",
        \"version\": \"0.1.0\"
    },
    {
        \"name\": \"WebAppData\",
        \"documentation\": \"\",
        \"nature\": \"type_definition\",
        \"metadata\": {
            \"deprecation\": null
        },
        \"json_signature\": {
            \"name\": \"WebAppData\",
            \"type\": \"type-definition\",
            \"parameters\": 0,
            \"deprecation\": null,
            \"constructors\": [
                {
                    \"name\": \"WebAppData\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": {
                                \"ref\": null,
                                \"name\": \"String\",
                                \"type\": \"named\",
                                \"module\": \"gleam\",
                                \"package\": \"\",
                                \"parameters\": []
                            },
                            \"label\": \"data\"
                        },
                        {
                            \"type\": {
                                \"ref\": null,
                                \"name\": \"String\",
                                \"type\": \"named\",
                                \"module\": \"gleam\",
                                \"package\": \"\",
                                \"parameters\": []
                            },
                            \"label\": \"button_text\"
                        }
                    ],
                    \"documentation\": \" Describes data sent from a [Web App](https://core.telegram.org/bots/webapps) to the bot.\"
                }
            ],
            \"documentation\": null
        },
        \"module_name\": \"telega/model\",
        \"package_name\": \"telega\",
        \"version\": \"0.1.0\"
    },
    {
        \"name\": \"bg_app\",
        \"documentation\": \"\",
        \"nature\": \"function\",
        \"metadata\": {
            \"deprecation\": null,
            \"implementations\": {
                \"gleam\": false,
                \"uses_erlang_externals\": true,
                \"uses_javascript_externals\": true
            }
        },
        \"json_signature\": {
            \"name\": \"bg_app\",
            \"type\": \"function\",
            \"return\": {
                \"ref\": \"3.1.4\",
                \"name\": \"Attribute\",
                \"type\": \"named\",
                \"module\": \"lustre/attribute\",
                \"package\": \"lustre\",
                \"parameters\": [
                    {
                        \"id\": 0,
                        \"type\": \"variable\"
                    }
                ]
            },
            \"parameters\": [],
            \"deprecation\": null,
            \"documentation\": null,
            \"implementations\": {
                \"gleam\": false,
                \"uses_erlang_externals\": true,
                \"uses_javascript_externals\": true
            }
        },
        \"module_name\": \"lustre/ui/classes\",
        \"package_name\": \"lustre_ui\",
        \"version\": \"0.3.0\"
    }
]"
  |> json.decode(search_result.decode_search_results)
}
