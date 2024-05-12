import data/decoders/search_result
import gleam/json

pub fn mock() {
  "[
    {
        \"name\": \"decode_application\",
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
            \"name\": \"decode_application\",
            \"type\": \"function\",
            \"return\": {
                \"ref\": null,
                \"name\": \"Result\",
                \"type\": \"named\",
                \"module\": \"gleam\",
                \"package\": \"\",
                \"parameters\": [
                    {
                        \"ref\": \"0.0.1\",
                        \"name\": \"Application\",
                        \"type\": \"named\",
                        \"module\": \"models/api\",
                        \"package\": \"glyph\",
                        \"parameters\": []
                    },
                    {
                        \"ref\": \"1.0.1\",
                        \"name\": \"DecodeError\",
                        \"type\": \"named\",
                        \"module\": \"gleam/json\",
                        \"package\": \"gleam_json\",
                        \"parameters\": []
                    }
                ]
            },
            \"parameters\": [
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
                        \"ref\": null,
                        \"name\": \"String\",
                        \"type\": \"named\",
                        \"module\": \"gleam\",
                        \"package\": \"\",
                        \"parameters\": []
                    }
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
        \"module_name\": \"network/rest\",
        \"package_name\": \"glyph\",
        \"version\": \"0.0.1\"
    },
    {
        \"name\": \"application\",
        \"documentation\": \"Creates the application debugger from the tardis. Should be run once,\\n at the start of the application. It can be skipped when using [`single`](#single).\",
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
            \"name\": \"application\",
            \"type\": \"function\",
            \"return\": {
                \"ref\": \"0.1.0\",
                \"name\": \"Instance\",
                \"type\": \"named\",
                \"module\": \"tardis\",
                \"package\": \"tardis\",
                \"parameters\": []
            },
            \"parameters\": [
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
                        \"ref\": \"0.1.0\",
                        \"name\": \"Tardis\",
                        \"type\": \"named\",
                        \"module\": \"tardis\",
                        \"package\": \"tardis\",
                        \"parameters\": []
                    }
                },
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
                        \"ref\": null,
                        \"name\": \"String\",
                        \"type\": \"named\",
                        \"module\": \"gleam\",
                        \"package\": \"\",
                        \"parameters\": []
                    }
                }
            ],
            \"deprecation\": \" Creates the application debugger from the tardis. Should be run once,\\n at the start of the application. It can be skipped when using [`single`](#single).\",
            \"documentation\": \" Creates the application debugger from the tardis. Should be run once,\\n at the start of the application. It can be skipped when using [`single`](#single).\",
            \"implementations\": {
                \"gleam\": false,
                \"uses_erlang_externals\": true,
                \"uses_javascript_externals\": true
            }
        },
        \"module_name\": \"tardis\",
        \"package_name\": \"tardis\",
        \"version\": \"0.1.0\"
    },
    {
        \"name\": \"application\",
        \"documentation\": \"A complete Lustre application that follows the Model-View-Update architecture\\n and can handle side effects like HTTP requests or querying the DOM. Most real\\n Lustre applications will use this constructor.\\n\\n To learn more about effects and their purpose, take a look at the\\n [`effect`](./lustre/effect) module or the\\n [HTTP requests example](https://github.com/lustre-labs/lustre/tree/main/examples/05-http-requests).\",
        \"nature\": \"function\",
        \"metadata\": {
            \"deprecation\": null,
            \"implementations\": {
                \"gleam\": true,
                \"uses_erlang_externals\": false,
                \"uses_javascript_externals\": false
            }
        },
        \"json_signature\": {
            \"name\": \"application\",
            \"type\": \"function\",
            \"return\": {
                \"ref\": \"4.1.3\",
                \"name\": \"App\",
                \"type\": \"named\",
                \"module\": \"lustre\",
                \"package\": \"lustre\",
                \"parameters\": [
                    {
                        \"id\": 0,
                        \"type\": \"variable\"
                    },
                    {
                        \"id\": 1,
                        \"type\": \"variable\"
                    },
                    {
                        \"id\": 2,
                        \"type\": \"variable\"
                    }
                ]
            },
            \"parameters\": [
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
                        \"type\": \"fn\",
                        \"params\": [
                            {
                                \"id\": 0,
                                \"type\": \"variable\"
                            }
                        ],
                        \"return\": {
                            \"type\": \"tuple\",
                            \"elements\": [
                                {
                                    \"id\": 1,
                                    \"type\": \"variable\"
                                },
                                {
                                    \"ref\": \"4.1.3\",
                                    \"name\": \"Effect\",
                                    \"type\": \"named\",
                                    \"module\": \"lustre/effect\",
                                    \"package\": \"lustre\",
                                    \"parameters\": [
                                        {
                                            \"id\": 2,
                                            \"type\": \"variable\"
                                        }
                                    ]
                                }
                            ]
                        }
                    }
                },
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
                        \"type\": \"fn\",
                        \"params\": [
                            {
                                \"id\": 1,
                                \"type\": \"variable\"
                            },
                            {
                                \"id\": 2,
                                \"type\": \"variable\"
                            }
                        ],
                        \"return\": {
                            \"type\": \"tuple\",
                            \"elements\": [
                                {
                                    \"id\": 1,
                                    \"type\": \"variable\"
                                },
                                {
                                    \"ref\": \"4.1.3\",
                                    \"name\": \"Effect\",
                                    \"type\": \"named\",
                                    \"module\": \"lustre/effect\",
                                    \"package\": \"lustre\",
                                    \"parameters\": [
                                        {
                                            \"id\": 2,
                                            \"type\": \"variable\"
                                        }
                                    ]
                                }
                            ]
                        }
                    }
                },
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
                        \"type\": \"fn\",
                        \"params\": [
                            {
                                \"id\": 1,
                                \"type\": \"variable\"
                            }
                        ],
                        \"return\": {
                            \"ref\": null,
                            \"name\": \"Element\",
                            \"type\": \"named\",
                            \"module\": \"lustre/internals/vdom\",
                            \"package\": \"lustre\",
                            \"parameters\": [
                                {
                                    \"id\": 2,
                                    \"type\": \"variable\"
                                }
                            ]
                        }
                    }
                }
            ],
            \"deprecation\": \" A complete Lustre application that follows the Model-View-Update architecture\\n and can handle side effects like HTTP requests or querying the DOM. Most real\\n Lustre applications will use this constructor.\\n\\n To learn more about effects and their purpose, take a look at the\\n [`effect`](./lustre/effect) module or the\\n [HTTP requests example](https://github.com/lustre-labs/lustre/tree/main/examples/05-http-requests).\\n\",
            \"documentation\": \" A complete Lustre application that follows the Model-View-Update architecture\\n and can handle side effects like HTTP requests or querying the DOM. Most real\\n Lustre applications will use this constructor.\\n\\n To learn more about effects and their purpose, take a look at the\\n [`effect`](./lustre/effect) module or the\\n [HTTP requests example](https://github.com/lustre-labs/lustre/tree/main/examples/05-http-requests).\\n\",
            \"implementations\": {
                \"gleam\": true,
                \"uses_erlang_externals\": false,
                \"uses_javascript_externals\": false
            }
        },
        \"module_name\": \"lustre\",
        \"package_name\": \"lustre\",
        \"version\": \"4.1.3\"
    },
    {
        \"name\": \"application\",
        \"documentation\": \"A complete Lustre application that follows the Model-View-Update architecture\\n and can handle side effects like HTTP requests or querying the DOM. Most real\\n Lustre applications will use this constructor.\\n\\n To learn more about effects and their purpose, take a look at the\\n [`effect`](./lustre/effect) module or the\\n [HTTP requests example](https://github.com/lustre-labs/lustre/tree/main/examples/05-http-requests).\",
        \"nature\": \"function\",
        \"metadata\": {
            \"deprecation\": null,
            \"implementations\": {
                \"gleam\": true,
                \"uses_erlang_externals\": false,
                \"uses_javascript_externals\": false
            }
        },
        \"json_signature\": {
            \"name\": \"application\",
            \"type\": \"function\",
            \"return\": {
                \"ref\": \"4.1.2\",
                \"name\": \"App\",
                \"type\": \"named\",
                \"module\": \"lustre\",
                \"package\": \"lustre\",
                \"parameters\": [
                    {
                        \"id\": 0,
                        \"type\": \"variable\"
                    },
                    {
                        \"id\": 1,
                        \"type\": \"variable\"
                    },
                    {
                        \"id\": 2,
                        \"type\": \"variable\"
                    }
                ]
            },
            \"parameters\": [
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
                        \"type\": \"fn\",
                        \"params\": [
                            {
                                \"id\": 0,
                                \"type\": \"variable\"
                            }
                        ],
                        \"return\": {
                            \"type\": \"tuple\",
                            \"elements\": [
                                {
                                    \"id\": 1,
                                    \"type\": \"variable\"
                                },
                                {
                                    \"ref\": \"4.1.2\",
                                    \"name\": \"Effect\",
                                    \"type\": \"named\",
                                    \"module\": \"lustre/effect\",
                                    \"package\": \"lustre\",
                                    \"parameters\": [
                                        {
                                            \"id\": 2,
                                            \"type\": \"variable\"
                                        }
                                    ]
                                }
                            ]
                        }
                    }
                },
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
                        \"type\": \"fn\",
                        \"params\": [
                            {
                                \"id\": 1,
                                \"type\": \"variable\"
                            },
                            {
                                \"id\": 2,
                                \"type\": \"variable\"
                            }
                        ],
                        \"return\": {
                            \"type\": \"tuple\",
                            \"elements\": [
                                {
                                    \"id\": 1,
                                    \"type\": \"variable\"
                                },
                                {
                                    \"ref\": \"4.1.2\",
                                    \"name\": \"Effect\",
                                    \"type\": \"named\",
                                    \"module\": \"lustre/effect\",
                                    \"package\": \"lustre\",
                                    \"parameters\": [
                                        {
                                            \"id\": 2,
                                            \"type\": \"variable\"
                                        }
                                    ]
                                }
                            ]
                        }
                    }
                },
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
                        \"type\": \"fn\",
                        \"params\": [
                            {
                                \"id\": 1,
                                \"type\": \"variable\"
                            }
                        ],
                        \"return\": {
                            \"ref\": null,
                            \"name\": \"Element\",
                            \"type\": \"named\",
                            \"module\": \"lustre/internals/vdom\",
                            \"package\": \"lustre\",
                            \"parameters\": [
                                {
                                    \"id\": 2,
                                    \"type\": \"variable\"
                                }
                            ]
                        }
                    }
                }
            ],
            \"deprecation\": \" A complete Lustre application that follows the Model-View-Update architecture\\n and can handle side effects like HTTP requests or querying the DOM. Most real\\n Lustre applications will use this constructor.\\n\\n To learn more about effects and their purpose, take a look at the\\n [`effect`](./lustre/effect) module or the\\n [HTTP requests example](https://github.com/lustre-labs/lustre/tree/main/examples/05-http-requests).\\n\",
            \"documentation\": \" A complete Lustre application that follows the Model-View-Update architecture\\n and can handle side effects like HTTP requests or querying the DOM. Most real\\n Lustre applications will use this constructor.\\n\\n To learn more about effects and their purpose, take a look at the\\n [`effect`](./lustre/effect) module or the\\n [HTTP requests example](https://github.com/lustre-labs/lustre/tree/main/examples/05-http-requests).\\n\",
            \"implementations\": {
                \"gleam\": true,
                \"uses_erlang_externals\": false,
                \"uses_javascript_externals\": false
            }
        },
        \"module_name\": \"lustre\",
        \"package_name\": \"lustre\",
        \"version\": \"4.1.2\"
    },
    {
        \"name\": \"application\",
        \"documentation\": \"A complete Lustre application that follows the Model-View-Update architecture\\n and can handle side effects like HTTP requests or querying the DOM. Most real\\n Lustre applications will use this constructor.\\n\\n To learn more about effects and their purpose, take a look at the\\n [`effect`](./lustre/effect) module or the\\n [HTTP requests example](https://github.com/lustre-labs/lustre/tree/main/examples/05-http-requests).\",
        \"nature\": \"function\",
        \"metadata\": {
            \"deprecation\": null,
            \"implementations\": {
                \"gleam\": true,
                \"uses_erlang_externals\": false,
                \"uses_javascript_externals\": false
            }
        },
        \"json_signature\": {
            \"name\": \"application\",
            \"type\": \"function\",
            \"return\": {
                \"ref\": \"4.0.0\",
                \"name\": \"App\",
                \"type\": \"named\",
                \"module\": \"lustre\",
                \"package\": \"lustre\",
                \"parameters\": [
                    {
                        \"id\": 0,
                        \"type\": \"variable\"
                    },
                    {
                        \"id\": 1,
                        \"type\": \"variable\"
                    },
                    {
                        \"id\": 2,
                        \"type\": \"variable\"
                    }
                ]
            },
            \"parameters\": [
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
                        \"type\": \"fn\",
                        \"params\": [
                            {
                                \"id\": 0,
                                \"type\": \"variable\"
                            }
                        ],
                        \"return\": {
                            \"type\": \"tuple\",
                            \"elements\": [
                                {
                                    \"id\": 1,
                                    \"type\": \"variable\"
                                },
                                {
                                    \"ref\": \"4.0.0\",
                                    \"name\": \"Effect\",
                                    \"type\": \"named\",
                                    \"module\": \"lustre/effect\",
                                    \"package\": \"lustre\",
                                    \"parameters\": [
                                        {
                                            \"id\": 2,
                                            \"type\": \"variable\"
                                        }
                                    ]
                                }
                            ]
                        }
                    }
                },
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
                        \"type\": \"fn\",
                        \"params\": [
                            {
                                \"id\": 1,
                                \"type\": \"variable\"
                            },
                            {
                                \"id\": 2,
                                \"type\": \"variable\"
                            }
                        ],
                        \"return\": {
                            \"type\": \"tuple\",
                            \"elements\": [
                                {
                                    \"id\": 1,
                                    \"type\": \"variable\"
                                },
                                {
                                    \"ref\": \"4.0.0\",
                                    \"name\": \"Effect\",
                                    \"type\": \"named\",
                                    \"module\": \"lustre/effect\",
                                    \"package\": \"lustre\",
                                    \"parameters\": [
                                        {
                                            \"id\": 2,
                                            \"type\": \"variable\"
                                        }
                                    ]
                                }
                            ]
                        }
                    }
                },
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
                        \"type\": \"fn\",
                        \"params\": [
                            {
                                \"id\": 1,
                                \"type\": \"variable\"
                            }
                        ],
                        \"return\": {
                            \"ref\": null,
                            \"name\": \"Element\",
                            \"type\": \"named\",
                            \"module\": \"lustre/internals/vdom\",
                            \"package\": \"lustre\",
                            \"parameters\": [
                                {
                                    \"id\": 2,
                                    \"type\": \"variable\"
                                }
                            ]
                        }
                    }
                }
            ],
            \"deprecation\": \" A complete Lustre application that follows the Model-View-Update architecture\\n and can handle side effects like HTTP requests or querying the DOM. Most real\\n Lustre applications will use this constructor.\\n\\n To learn more about effects and their purpose, take a look at the\\n [`effect`](./lustre/effect) module or the\\n [HTTP requests example](https://github.com/lustre-labs/lustre/tree/main/examples/05-http-requests).\\n\",
            \"documentation\": \" A complete Lustre application that follows the Model-View-Update architecture\\n and can handle side effects like HTTP requests or querying the DOM. Most real\\n Lustre applications will use this constructor.\\n\\n To learn more about effects and their purpose, take a look at the\\n [`effect`](./lustre/effect) module or the\\n [HTTP requests example](https://github.com/lustre-labs/lustre/tree/main/examples/05-http-requests).\\n\",
            \"implementations\": {
                \"gleam\": true,
                \"uses_erlang_externals\": false,
                \"uses_javascript_externals\": false
            }
        },
        \"module_name\": \"lustre\",
        \"package_name\": \"lustre\",
        \"version\": \"4.0.0\"
    },
    {
        \"name\": \"Body\",
        \"documentation\": \"The body of a HTTP response, to be sent to the client.\",
        \"nature\": \"type_definition\",
        \"metadata\": {
            \"deprecation\": null
        },
        \"json_signature\": {
            \"name\": \"Body\",
            \"type\": \"type-definition\",
            \"parameters\": 0,
            \"deprecation\": \" The body of a HTTP response, to be sent to the client.\\n\",
            \"constructors\": [
                {
                    \"name\": \"Text\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": null,
                            \"params_type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"StringBuilder\",
                                \"type\": \"named\",
                                \"module\": \"gleam/string_builder\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": []
                            }
                        }
                    ],
                    \"documentation\": \" A body of unicode text.\\n\\n The body is represented using a `StringBuilder`. If you have a `String`\\n you can use the `string_builder.from_string` function to convert it.\\n\"
                },
                {
                    \"name\": \"Bytes\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": null,
                            \"params_type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"BytesBuilder\",
                                \"type\": \"named\",
                                \"module\": \"gleam/bytes_builder\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": []
                            }
                        }
                    ],
                    \"documentation\": \" A body of binary data.\\n\\n The body is represented using a `StringBuilder`. If you have a `String`\\n you can use the `string_builder.from_string` function to convert it.\\n\"
                },
                {
                    \"name\": \"File\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": \"path\",
                            \"params_type\": {
                                \"ref\": null,
                                \"name\": \"String\",
                                \"type\": \"named\",
                                \"module\": \"gleam\",
                                \"package\": \"\",
                                \"parameters\": []
                            }
                        }
                    ],
                    \"documentation\": \" A body of the contents of a file.\\n\\n This will be sent efficiently using the `send_file` function of the\\n underlying HTTP server. The file will not be read into memory so it is\\n safe to send large files this way.\\n\"
                },
                {
                    \"name\": \"Empty\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [],
                    \"documentation\": \" An empty body. This may be returned by the `require_*` middleware\\n functions in the event of a failure, invalid request, or other situation\\n in which the request cannot be processed.\\n\\n Your application may wish to use a middleware to provide default responses\\n in place of any with an empty body.\\n\"
                }
            ],
            \"documentation\": \" The body of a HTTP response, to be sent to the client.\\n\"
        },
        \"module_name\": \"wisp\",
        \"package_name\": \"wisp\",
        \"version\": \"0.13.0\"
    },
    {
        \"name\": \"application\",
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
            \"name\": \"application\",
            \"type\": \"function\",
            \"return\": {
                \"ref\": \"3.1.2\",
                \"name\": \"App\",
                \"type\": \"named\",
                \"module\": \"lustre\",
                \"package\": \"lustre\",
                \"parameters\": [
                    {
                        \"id\": 0,
                        \"type\": \"variable\"
                    },
                    {
                        \"id\": 1,
                        \"type\": \"variable\"
                    },
                    {
                        \"id\": 2,
                        \"type\": \"variable\"
                    }
                ]
            },
            \"parameters\": [
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
                        \"type\": \"fn\",
                        \"params\": [
                            {
                                \"id\": 0,
                                \"type\": \"variable\"
                            }
                        ],
                        \"return\": {
                            \"type\": \"tuple\",
                            \"elements\": [
                                {
                                    \"id\": 1,
                                    \"type\": \"variable\"
                                },
                                {
                                    \"ref\": \"3.1.2\",
                                    \"name\": \"Effect\",
                                    \"type\": \"named\",
                                    \"module\": \"lustre/effect\",
                                    \"package\": \"lustre\",
                                    \"parameters\": [
                                        {
                                            \"id\": 2,
                                            \"type\": \"variable\"
                                        }
                                    ]
                                }
                            ]
                        }
                    }
                },
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
                        \"type\": \"fn\",
                        \"params\": [
                            {
                                \"id\": 1,
                                \"type\": \"variable\"
                            },
                            {
                                \"id\": 2,
                                \"type\": \"variable\"
                            }
                        ],
                        \"return\": {
                            \"type\": \"tuple\",
                            \"elements\": [
                                {
                                    \"id\": 1,
                                    \"type\": \"variable\"
                                },
                                {
                                    \"ref\": \"3.1.2\",
                                    \"name\": \"Effect\",
                                    \"type\": \"named\",
                                    \"module\": \"lustre/effect\",
                                    \"package\": \"lustre\",
                                    \"parameters\": [
                                        {
                                            \"id\": 2,
                                            \"type\": \"variable\"
                                        }
                                    ]
                                }
                            ]
                        }
                    }
                },
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
                        \"type\": \"fn\",
                        \"params\": [
                            {
                                \"id\": 1,
                                \"type\": \"variable\"
                            }
                        ],
                        \"return\": {
                            \"ref\": \"3.1.2\",
                            \"name\": \"Element\",
                            \"type\": \"named\",
                            \"module\": \"lustre/element\",
                            \"package\": \"lustre\",
                            \"parameters\": [
                                {
                                    \"id\": 2,
                                    \"type\": \"variable\"
                                }
                            ]
                        }
                    }
                }
            ],
            \"deprecation\": \"\",
            \"documentation\": \"\",
            \"implementations\": {
                \"gleam\": false,
                \"uses_erlang_externals\": true,
                \"uses_javascript_externals\": true
            }
        },
        \"module_name\": \"lustre\",
        \"package_name\": \"lustre\",
        \"version\": \"3.1.2\"
    },
    {
        \"name\": \"Application\",
        \"documentation\": \"Model for a Discord Application: https://discord.com/developers/docs/resources/application\\n Note to self: summary is deprecated and will be removed in v11\",
        \"nature\": \"type_definition\",
        \"metadata\": {
            \"deprecation\": null
        },
        \"json_signature\": {
            \"name\": \"Application\",
            \"type\": \"type-definition\",
            \"parameters\": 0,
            \"deprecation\": \" Model for a Discord Application: https://discord.com/developers/docs/resources/application\\n Note to self: summary is deprecated and will be removed in v11\",
            \"constructors\": [
                {
                    \"name\": \"Application\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": \"id\",
                            \"params_type\": {
                                \"ref\": null,
                                \"name\": \"String\",
                                \"type\": \"named\",
                                \"module\": \"gleam\",
                                \"package\": \"\",
                                \"parameters\": []
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"name\",
                            \"params_type\": {
                                \"ref\": null,
                                \"name\": \"String\",
                                \"type\": \"named\",
                                \"module\": \"gleam\",
                                \"package\": \"\",
                                \"parameters\": []
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"icon\",
                            \"params_type\": {
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
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"description\",
                            \"params_type\": {
                                \"ref\": null,
                                \"name\": \"String\",
                                \"type\": \"named\",
                                \"module\": \"gleam\",
                                \"package\": \"\",
                                \"parameters\": []
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"rpc_origins\",
                            \"params_type\": {
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
                                                \"ref\": null,
                                                \"name\": \"String\",
                                                \"type\": \"named\",
                                                \"module\": \"gleam\",
                                                \"package\": \"\",
                                                \"parameters\": []
                                            }
                                        ]
                                    }
                                ]
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"bot_public\",
                            \"params_type\": {
                                \"ref\": null,
                                \"name\": \"Bool\",
                                \"type\": \"named\",
                                \"module\": \"gleam\",
                                \"package\": \"\",
                                \"parameters\": []
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"bot_require_code_grant\",
                            \"params_type\": {
                                \"ref\": null,
                                \"name\": \"Bool\",
                                \"type\": \"named\",
                                \"module\": \"gleam\",
                                \"package\": \"\",
                                \"parameters\": []
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"bot\",
                            \"params_type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": \"0.0.1\",
                                        \"name\": \"User\",
                                        \"type\": \"named\",
                                        \"module\": \"models/api\",
                                        \"package\": \"glyph\",
                                        \"parameters\": []
                                    }
                                ]
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"terms_of_service_url\",
                            \"params_type\": {
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
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"privacy_policy_url\",
                            \"params_type\": {
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
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"owner\",
                            \"params_type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": \"0.0.1\",
                                        \"name\": \"User\",
                                        \"type\": \"named\",
                                        \"module\": \"models/api\",
                                        \"package\": \"glyph\",
                                        \"parameters\": []
                                    }
                                ]
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"summary\",
                            \"params_type\": {
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
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"verify_key\",
                            \"params_type\": {
                                \"ref\": null,
                                \"name\": \"String\",
                                \"type\": \"named\",
                                \"module\": \"gleam\",
                                \"package\": \"\",
                                \"parameters\": []
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"team\",
                            \"params_type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": \"0.0.1\",
                                        \"name\": \"Team\",
                                        \"type\": \"named\",
                                        \"module\": \"models/api\",
                                        \"package\": \"glyph\",
                                        \"parameters\": []
                                    }
                                ]
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"guild_id\",
                            \"params_type\": {
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
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"primary_sku_id\",
                            \"params_type\": {
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
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"slug\",
                            \"params_type\": {
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
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"cover_image\",
                            \"params_type\": {
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
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"flags\",
                            \"params_type\": {
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
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"approximate_guild_count\",
                            \"params_type\": {
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
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"redirect_uris\",
                            \"params_type\": {
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
                                                \"ref\": null,
                                                \"name\": \"String\",
                                                \"type\": \"named\",
                                                \"module\": \"gleam\",
                                                \"package\": \"\",
                                                \"parameters\": []
                                            }
                                        ]
                                    }
                                ]
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"interactions_endpoint_url\",
                            \"params_type\": {
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
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"role_connections_verification_url\",
                            \"params_type\": {
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
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"tags\",
                            \"params_type\": {
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
                                                \"ref\": null,
                                                \"name\": \"String\",
                                                \"type\": \"named\",
                                                \"module\": \"gleam\",
                                                \"package\": \"\",
                                                \"parameters\": []
                                            }
                                        ]
                                    }
                                ]
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"install_params\",
                            \"params_type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": \"0.0.1\",
                                        \"name\": \"InstallParams\",
                                        \"type\": \"named\",
                                        \"module\": \"models/api\",
                                        \"package\": \"glyph\",
                                        \"parameters\": []
                                    }
                                ]
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"custom_install_url\",
                            \"params_type\": {
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
                            }
                        }
                    ],
                    \"documentation\": null
                }
            ],
            \"documentation\": \" Model for a Discord Application: https://discord.com/developers/docs/resources/application\\n Note to self: summary is deprecated and will be removed in v11\"
        },
        \"module_name\": \"models/api\",
        \"package_name\": \"glyph\",
        \"version\": \"0.0.1\"
    },
    {
        \"name\": \"Body\",
        \"documentation\": \"The body of a HTTP response, to be sent to the client.\",
        \"nature\": \"type_definition\",
        \"metadata\": {
            \"deprecation\": null
        },
        \"json_signature\": {
            \"name\": \"Body\",
            \"type\": \"type-definition\",
            \"parameters\": 0,
            \"deprecation\": \" The body of a HTTP response, to be sent to the client.\\n\",
            \"constructors\": [
                {
                    \"name\": \"Text\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": null,
                            \"params_type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"StringBuilder\",
                                \"type\": \"named\",
                                \"module\": \"gleam/string_builder\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": []
                            }
                        }
                    ],
                    \"documentation\": \" A body of unicode text.\\n\\n The body is represented using a `StringBuilder`. If you have a `String`\\n you can use the `string_builder.from_string` function to convert it.\\n\"
                },
                {
                    \"name\": \"File\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": \"path\",
                            \"params_type\": {
                                \"ref\": null,
                                \"name\": \"String\",
                                \"type\": \"named\",
                                \"module\": \"gleam\",
                                \"package\": \"\",
                                \"parameters\": []
                            }
                        }
                    ],
                    \"documentation\": \" A body of the contents of a file.\\n\\n This will be sent efficiently using the `send_file` function of the\\n underlying HTTP server. The file will not be read into memory so it is\\n safe to send large files this way.\\n\"
                },
                {
                    \"name\": \"Empty\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [],
                    \"documentation\": \" An empty body. This may be returned by the `require_*` middleware\\n functions in the event of a failure, invalid request, or other situation\\n in which the request cannot be processed.\\n\\n Your application may wish to use a middleware to provide default responses\\n in place of any with an empty body.\\n\"
                }
            ],
            \"documentation\": \" The body of a HTTP response, to be sent to the client.\\n\"
        },
        \"module_name\": \"wisp\",
        \"package_name\": \"wisp\",
        \"version\": \"0.11.0\"
    },
    {
        \"name\": \"Body\",
        \"documentation\": \"The body of a HTTP response, to be sent to the client.\",
        \"nature\": \"type_definition\",
        \"metadata\": {
            \"deprecation\": null
        },
        \"json_signature\": {
            \"name\": \"Body\",
            \"type\": \"type-definition\",
            \"parameters\": 0,
            \"deprecation\": \" The body of a HTTP response, to be sent to the client.\\n\",
            \"constructors\": [
                {
                    \"name\": \"Text\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": null,
                            \"params_type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"StringBuilder\",
                                \"type\": \"named\",
                                \"module\": \"gleam/string_builder\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": []
                            }
                        }
                    ],
                    \"documentation\": \" A body of unicode text.\\n\\n The body is represented using a `StringBuilder`. If you have a `String`\\n you can use the `string_builder.from_string` function to convert it.\\n\"
                },
                {
                    \"name\": \"File\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": \"path\",
                            \"params_type\": {
                                \"ref\": null,
                                \"name\": \"String\",
                                \"type\": \"named\",
                                \"module\": \"gleam\",
                                \"package\": \"\",
                                \"parameters\": []
                            }
                        }
                    ],
                    \"documentation\": \" A body of the contents of a file.\\n\\n This will be sent efficiently using the `send_file` function of the\\n underlying HTTP server. The file will not be read into memory so it is\\n safe to send large files this way.\\n\"
                },
                {
                    \"name\": \"Empty\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [],
                    \"documentation\": \" An empty body. This may be returned by the `require_*` middleware\\n functions in the event of a failure, invalid request, or other situation\\n in which the request cannot be processed.\\n\\n Your application may wish to use a middleware to provide default responses\\n in place of any with an empty body.\\n\"
                }
            ],
            \"documentation\": \" The body of a HTTP response, to be sent to the client.\\n\"
        },
        \"module_name\": \"wisp\",
        \"package_name\": \"wisp\",
        \"version\": \"0.10.0\"
    },
    {
        \"name\": \"get_application\",
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
            \"name\": \"get_application\",
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
                        \"name\": \"Application\",
                        \"type\": \"named\",
                        \"module\": \"glyph/models/discord\",
                        \"package\": \"glyph\",
                        \"parameters\": []
                    },
                    {
                        \"ref\": \"0.1.0\",
                        \"name\": \"APIError\",
                        \"type\": \"named\",
                        \"module\": \"glyph/clients/api\",
                        \"package\": \"glyph\",
                        \"parameters\": []
                    }
                ]
            },
            \"parameters\": [
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
                        \"ref\": \"0.1.0\",
                        \"name\": \"APIClient\",
                        \"type\": \"named\",
                        \"module\": \"glyph/clients/api\",
                        \"package\": \"glyph\",
                        \"parameters\": []
                    }
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
        \"module_name\": \"glyph/clients/api\",
        \"package_name\": \"glyph\",
        \"version\": \"0.1.0\"
    },
    {
        \"name\": \"application\",
        \"documentation\": \"A complete Lustre application that follows the Model-View-Update architecture\\n and can handle side effects like HTTP requests or querying the DOM. Most real\\n Lustre applications will use this constructor.\\n\\n To learn more about effects and their purpose, take a look at the\\n [`effect`](./lustre/effect) module or the\\n [HTTP requests example](https://github.com/lustre-labs/lustre/tree/main/examples/05-http-requests).\",
        \"nature\": \"function\",
        \"metadata\": {
            \"deprecation\": null,
            \"implementations\": {
                \"gleam\": true,
                \"uses_erlang_externals\": false,
                \"uses_javascript_externals\": false
            }
        },
        \"json_signature\": {
            \"name\": \"application\",
            \"type\": \"function\",
            \"return\": {
                \"ref\": \"4.1.5\",
                \"name\": \"App\",
                \"type\": \"named\",
                \"module\": \"lustre\",
                \"package\": \"lustre\",
                \"parameters\": [
                    {
                        \"id\": 0,
                        \"type\": \"variable\"
                    },
                    {
                        \"id\": 1,
                        \"type\": \"variable\"
                    },
                    {
                        \"id\": 2,
                        \"type\": \"variable\"
                    }
                ]
            },
            \"parameters\": [
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
                        \"type\": \"fn\",
                        \"params\": [
                            {
                                \"id\": 0,
                                \"type\": \"variable\"
                            }
                        ],
                        \"return\": {
                            \"type\": \"tuple\",
                            \"elements\": [
                                {
                                    \"id\": 1,
                                    \"type\": \"variable\"
                                },
                                {
                                    \"ref\": \"4.1.5\",
                                    \"name\": \"Effect\",
                                    \"type\": \"named\",
                                    \"module\": \"lustre/effect\",
                                    \"package\": \"lustre\",
                                    \"parameters\": [
                                        {
                                            \"id\": 2,
                                            \"type\": \"variable\"
                                        }
                                    ]
                                }
                            ]
                        }
                    }
                },
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
                        \"type\": \"fn\",
                        \"params\": [
                            {
                                \"id\": 1,
                                \"type\": \"variable\"
                            },
                            {
                                \"id\": 2,
                                \"type\": \"variable\"
                            }
                        ],
                        \"return\": {
                            \"type\": \"tuple\",
                            \"elements\": [
                                {
                                    \"id\": 1,
                                    \"type\": \"variable\"
                                },
                                {
                                    \"ref\": \"4.1.5\",
                                    \"name\": \"Effect\",
                                    \"type\": \"named\",
                                    \"module\": \"lustre/effect\",
                                    \"package\": \"lustre\",
                                    \"parameters\": [
                                        {
                                            \"id\": 2,
                                            \"type\": \"variable\"
                                        }
                                    ]
                                }
                            ]
                        }
                    }
                },
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
                        \"type\": \"fn\",
                        \"params\": [
                            {
                                \"id\": 1,
                                \"type\": \"variable\"
                            }
                        ],
                        \"return\": {
                            \"ref\": null,
                            \"name\": \"Element\",
                            \"type\": \"named\",
                            \"module\": \"lustre/internals/vdom\",
                            \"package\": \"lustre\",
                            \"parameters\": [
                                {
                                    \"id\": 2,
                                    \"type\": \"variable\"
                                }
                            ]
                        }
                    }
                }
            ],
            \"deprecation\": \" A complete Lustre application that follows the Model-View-Update architecture\\n and can handle side effects like HTTP requests or querying the DOM. Most real\\n Lustre applications will use this constructor.\\n\\n To learn more about effects and their purpose, take a look at the\\n [`effect`](./lustre/effect) module or the\\n [HTTP requests example](https://github.com/lustre-labs/lustre/tree/main/examples/05-http-requests).\\n\",
            \"documentation\": \" A complete Lustre application that follows the Model-View-Update architecture\\n and can handle side effects like HTTP requests or querying the DOM. Most real\\n Lustre applications will use this constructor.\\n\\n To learn more about effects and their purpose, take a look at the\\n [`effect`](./lustre/effect) module or the\\n [HTTP requests example](https://github.com/lustre-labs/lustre/tree/main/examples/05-http-requests).\\n\",
            \"implementations\": {
                \"gleam\": true,
                \"uses_erlang_externals\": false,
                \"uses_javascript_externals\": false
            }
        },
        \"module_name\": \"lustre\",
        \"package_name\": \"lustre\",
        \"version\": \"4.1.5\"
    },
    {
        \"name\": \"application_stopped\",
        \"documentation\": \"\",
        \"nature\": \"function\",
        \"metadata\": {
            \"deprecation\": null,
            \"implementations\": {
                \"gleam\": false,
                \"uses_erlang_externals\": true,
                \"uses_javascript_externals\": false
            }
        },
        \"json_signature\": {
            \"name\": \"application_stopped\",
            \"type\": \"function\",
            \"return\": {
                \"ref\": \"0.10.0\",
                \"name\": \"ApplicationStop\",
                \"type\": \"named\",
                \"module\": \"gleam/otp/supervisor\",
                \"package\": \"gleam_otp\",
                \"parameters\": []
            },
            \"parameters\": [],
            \"deprecation\": null,
            \"documentation\": null,
            \"implementations\": {
                \"gleam\": false,
                \"uses_erlang_externals\": true,
                \"uses_javascript_externals\": false
            }
        },
        \"module_name\": \"gleam/otp/supervisor\",
        \"package_name\": \"gleam_otp\",
        \"version\": \"0.10.0\"
    },
    {
        \"name\": \"application\",
        \"documentation\": \"A complete Lustre application that follows the Model-View-Update architecture\\n and can handle side effects like HTTP requests or querying the DOM. Most real\\n Lustre applications will use this constructor.\\n\\n To learn more about effects and their purpose, take a look at the\\n [`effect`](./lustre/effect) module or the\\n [HTTP requests example](https://github.com/lustre-labs/lustre/tree/main/examples/05-http-requests).\",
        \"nature\": \"function\",
        \"metadata\": {
            \"deprecation\": null,
            \"implementations\": {
                \"gleam\": true,
                \"uses_erlang_externals\": false,
                \"uses_javascript_externals\": false
            }
        },
        \"json_signature\": {
            \"name\": \"application\",
            \"type\": \"function\",
            \"return\": {
                \"ref\": \"4.1.1\",
                \"name\": \"App\",
                \"type\": \"named\",
                \"module\": \"lustre\",
                \"package\": \"lustre\",
                \"parameters\": [
                    {
                        \"id\": 0,
                        \"type\": \"variable\"
                    },
                    {
                        \"id\": 1,
                        \"type\": \"variable\"
                    },
                    {
                        \"id\": 2,
                        \"type\": \"variable\"
                    }
                ]
            },
            \"parameters\": [
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
                        \"type\": \"fn\",
                        \"params\": [
                            {
                                \"id\": 0,
                                \"type\": \"variable\"
                            }
                        ],
                        \"return\": {
                            \"type\": \"tuple\",
                            \"elements\": [
                                {
                                    \"id\": 1,
                                    \"type\": \"variable\"
                                },
                                {
                                    \"ref\": \"4.1.1\",
                                    \"name\": \"Effect\",
                                    \"type\": \"named\",
                                    \"module\": \"lustre/effect\",
                                    \"package\": \"lustre\",
                                    \"parameters\": [
                                        {
                                            \"id\": 2,
                                            \"type\": \"variable\"
                                        }
                                    ]
                                }
                            ]
                        }
                    }
                },
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
                        \"type\": \"fn\",
                        \"params\": [
                            {
                                \"id\": 1,
                                \"type\": \"variable\"
                            },
                            {
                                \"id\": 2,
                                \"type\": \"variable\"
                            }
                        ],
                        \"return\": {
                            \"type\": \"tuple\",
                            \"elements\": [
                                {
                                    \"id\": 1,
                                    \"type\": \"variable\"
                                },
                                {
                                    \"ref\": \"4.1.1\",
                                    \"name\": \"Effect\",
                                    \"type\": \"named\",
                                    \"module\": \"lustre/effect\",
                                    \"package\": \"lustre\",
                                    \"parameters\": [
                                        {
                                            \"id\": 2,
                                            \"type\": \"variable\"
                                        }
                                    ]
                                }
                            ]
                        }
                    }
                },
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
                        \"type\": \"fn\",
                        \"params\": [
                            {
                                \"id\": 1,
                                \"type\": \"variable\"
                            }
                        ],
                        \"return\": {
                            \"ref\": null,
                            \"name\": \"Element\",
                            \"type\": \"named\",
                            \"module\": \"lustre/internals/vdom\",
                            \"package\": \"lustre\",
                            \"parameters\": [
                                {
                                    \"id\": 2,
                                    \"type\": \"variable\"
                                }
                            ]
                        }
                    }
                }
            ],
            \"deprecation\": \" A complete Lustre application that follows the Model-View-Update architecture\\n and can handle side effects like HTTP requests or querying the DOM. Most real\\n Lustre applications will use this constructor.\\n\\n To learn more about effects and their purpose, take a look at the\\n [`effect`](./lustre/effect) module or the\\n [HTTP requests example](https://github.com/lustre-labs/lustre/tree/main/examples/05-http-requests).\\n\",
            \"documentation\": \" A complete Lustre application that follows the Model-View-Update architecture\\n and can handle side effects like HTTP requests or querying the DOM. Most real\\n Lustre applications will use this constructor.\\n\\n To learn more about effects and their purpose, take a look at the\\n [`effect`](./lustre/effect) module or the\\n [HTTP requests example](https://github.com/lustre-labs/lustre/tree/main/examples/05-http-requests).\\n\",
            \"implementations\": {
                \"gleam\": true,
                \"uses_erlang_externals\": false,
                \"uses_javascript_externals\": false
            }
        },
        \"module_name\": \"lustre\",
        \"package_name\": \"lustre\",
        \"version\": \"4.1.1\"
    },
    {
        \"name\": \"application\",
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
            \"name\": \"application\",
            \"type\": \"function\",
            \"return\": {
                \"ref\": \"3.1.1\",
                \"name\": \"App\",
                \"type\": \"named\",
                \"module\": \"lustre\",
                \"package\": \"lustre\",
                \"parameters\": [
                    {
                        \"id\": 0,
                        \"type\": \"variable\"
                    },
                    {
                        \"id\": 1,
                        \"type\": \"variable\"
                    },
                    {
                        \"id\": 2,
                        \"type\": \"variable\"
                    }
                ]
            },
            \"parameters\": [
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
                        \"type\": \"fn\",
                        \"params\": [
                            {
                                \"id\": 0,
                                \"type\": \"variable\"
                            }
                        ],
                        \"return\": {
                            \"type\": \"tuple\",
                            \"elements\": [
                                {
                                    \"id\": 1,
                                    \"type\": \"variable\"
                                },
                                {
                                    \"ref\": \"3.1.1\",
                                    \"name\": \"Effect\",
                                    \"type\": \"named\",
                                    \"module\": \"lustre/effect\",
                                    \"package\": \"lustre\",
                                    \"parameters\": [
                                        {
                                            \"id\": 2,
                                            \"type\": \"variable\"
                                        }
                                    ]
                                }
                            ]
                        }
                    }
                },
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
                        \"type\": \"fn\",
                        \"params\": [
                            {
                                \"id\": 1,
                                \"type\": \"variable\"
                            },
                            {
                                \"id\": 2,
                                \"type\": \"variable\"
                            }
                        ],
                        \"return\": {
                            \"type\": \"tuple\",
                            \"elements\": [
                                {
                                    \"id\": 1,
                                    \"type\": \"variable\"
                                },
                                {
                                    \"ref\": \"3.1.1\",
                                    \"name\": \"Effect\",
                                    \"type\": \"named\",
                                    \"module\": \"lustre/effect\",
                                    \"package\": \"lustre\",
                                    \"parameters\": [
                                        {
                                            \"id\": 2,
                                            \"type\": \"variable\"
                                        }
                                    ]
                                }
                            ]
                        }
                    }
                },
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
                        \"type\": \"fn\",
                        \"params\": [
                            {
                                \"id\": 1,
                                \"type\": \"variable\"
                            }
                        ],
                        \"return\": {
                            \"ref\": \"3.1.1\",
                            \"name\": \"Element\",
                            \"type\": \"named\",
                            \"module\": \"lustre/element\",
                            \"package\": \"lustre\",
                            \"parameters\": [
                                {
                                    \"id\": 2,
                                    \"type\": \"variable\"
                                }
                            ]
                        }
                    }
                }
            ],
            \"deprecation\": \"\",
            \"documentation\": \"\",
            \"implementations\": {
                \"gleam\": false,
                \"uses_erlang_externals\": true,
                \"uses_javascript_externals\": true
            }
        },
        \"module_name\": \"lustre\",
        \"package_name\": \"lustre\",
        \"version\": \"3.1.1\"
    },
    {
        \"name\": \"application\",
        \"documentation\": \"A complete Lustre application that follows the Model-View-Update architecture\\n and can handle side effects like HTTP requests or querying the DOM. Most real\\n Lustre applications will use this constructor.\\n\\n To learn more about effects and their purpose, take a look at the\\n [`effect`](./lustre/effect.html) module or the\\n [HTTP requests example](https://github.com/lustre-labs/lustre/tree/main/examples/05-http-requests).\",
        \"nature\": \"function\",
        \"metadata\": {
            \"deprecation\": null,
            \"implementations\": {
                \"gleam\": true,
                \"uses_erlang_externals\": false,
                \"uses_javascript_externals\": false
            }
        },
        \"json_signature\": {
            \"name\": \"application\",
            \"type\": \"function\",
            \"return\": {
                \"ref\": \"4.1.8\",
                \"name\": \"App\",
                \"type\": \"named\",
                \"module\": \"lustre\",
                \"package\": \"lustre\",
                \"parameters\": [
                    {
                        \"id\": 0,
                        \"type\": \"variable\"
                    },
                    {
                        \"id\": 1,
                        \"type\": \"variable\"
                    },
                    {
                        \"id\": 2,
                        \"type\": \"variable\"
                    }
                ]
            },
            \"parameters\": [
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
                        \"type\": \"fn\",
                        \"params\": [
                            {
                                \"id\": 0,
                                \"type\": \"variable\"
                            }
                        ],
                        \"return\": {
                            \"type\": \"tuple\",
                            \"elements\": [
                                {
                                    \"id\": 1,
                                    \"type\": \"variable\"
                                },
                                {
                                    \"ref\": \"4.1.8\",
                                    \"name\": \"Effect\",
                                    \"type\": \"named\",
                                    \"module\": \"lustre/effect\",
                                    \"package\": \"lustre\",
                                    \"parameters\": [
                                        {
                                            \"id\": 2,
                                            \"type\": \"variable\"
                                        }
                                    ]
                                }
                            ]
                        }
                    }
                },
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
                        \"type\": \"fn\",
                        \"params\": [
                            {
                                \"id\": 1,
                                \"type\": \"variable\"
                            },
                            {
                                \"id\": 2,
                                \"type\": \"variable\"
                            }
                        ],
                        \"return\": {
                            \"type\": \"tuple\",
                            \"elements\": [
                                {
                                    \"id\": 1,
                                    \"type\": \"variable\"
                                },
                                {
                                    \"ref\": \"4.1.8\",
                                    \"name\": \"Effect\",
                                    \"type\": \"named\",
                                    \"module\": \"lustre/effect\",
                                    \"package\": \"lustre\",
                                    \"parameters\": [
                                        {
                                            \"id\": 2,
                                            \"type\": \"variable\"
                                        }
                                    ]
                                }
                            ]
                        }
                    }
                },
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
                        \"type\": \"fn\",
                        \"params\": [
                            {
                                \"id\": 1,
                                \"type\": \"variable\"
                            }
                        ],
                        \"return\": {
                            \"ref\": null,
                            \"name\": \"Element\",
                            \"type\": \"named\",
                            \"module\": \"lustre/internals/vdom\",
                            \"package\": \"lustre\",
                            \"parameters\": [
                                {
                                    \"id\": 2,
                                    \"type\": \"variable\"
                                }
                            ]
                        }
                    }
                }
            ],
            \"deprecation\": \" A complete Lustre application that follows the Model-View-Update architecture\\n and can handle side effects like HTTP requests or querying the DOM. Most real\\n Lustre applications will use this constructor.\\n\\n To learn more about effects and their purpose, take a look at the\\n [`effect`](./lustre/effect.html) module or the\\n [HTTP requests example](https://github.com/lustre-labs/lustre/tree/main/examples/05-http-requests).\\n\",
            \"documentation\": \" A complete Lustre application that follows the Model-View-Update architecture\\n and can handle side effects like HTTP requests or querying the DOM. Most real\\n Lustre applications will use this constructor.\\n\\n To learn more about effects and their purpose, take a look at the\\n [`effect`](./lustre/effect.html) module or the\\n [HTTP requests example](https://github.com/lustre-labs/lustre/tree/main/examples/05-http-requests).\\n\",
            \"implementations\": {
                \"gleam\": true,
                \"uses_erlang_externals\": false,
                \"uses_javascript_externals\": false
            }
        },
        \"module_name\": \"lustre\",
        \"package_name\": \"lustre\",
        \"version\": \"4.1.8\"
    },
    {
        \"name\": \"Body\",
        \"documentation\": \"The body of a HTTP response, to be sent to the client.\",
        \"nature\": \"type_definition\",
        \"metadata\": {
            \"deprecation\": null
        },
        \"json_signature\": {
            \"name\": \"Body\",
            \"type\": \"type-definition\",
            \"parameters\": 0,
            \"deprecation\": \" The body of a HTTP response, to be sent to the client.\\n\",
            \"constructors\": [
                {
                    \"name\": \"Text\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": null,
                            \"params_type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"StringBuilder\",
                                \"type\": \"named\",
                                \"module\": \"gleam/string_builder\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": []
                            }
                        }
                    ],
                    \"documentation\": \" A body of unicode text.\\n\\n The body is represented using a `StringBuilder`. If you have a `String`\\n you can use the `string_builder.from_string` function to convert it.\\n\"
                },
                {
                    \"name\": \"File\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": \"path\",
                            \"params_type\": {
                                \"ref\": null,
                                \"name\": \"String\",
                                \"type\": \"named\",
                                \"module\": \"gleam\",
                                \"package\": \"\",
                                \"parameters\": []
                            }
                        }
                    ],
                    \"documentation\": \" A body of the contents of a file.\\n\\n This will be sent efficiently using the `send_file` function of the\\n underlying HTTP server. The file will not be read into memory so it is\\n safe to send large files this way.\\n\"
                },
                {
                    \"name\": \"Empty\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [],
                    \"documentation\": \" An empty body. This may be returned by the `require_*` middleware\\n functions in the event of a failure, invalid request, or other situation\\n in which the request cannot be processed.\\n\\n Your application may wish to use a middleware to provide default responses\\n in place of any with an empty body.\\n\"
                }
            ],
            \"documentation\": \" The body of a HTTP response, to be sent to the client.\\n\"
        },
        \"module_name\": \"wisp\",
        \"package_name\": \"wisp\",
        \"version\": \"0.12.0\"
    },
    {
        \"name\": \"ensure_all_started\",
        \"documentation\": \"Starts an OTP application's process tree in the background, as well as\\n the trees of any applications that the given application depends upon. An\\n OTP application typically maps onto a Gleam or Hex package.\\n\\n Returns a list of the applications that were started. Calling this function\\n for application that have already been started is a no-op so you do not need\\n to check the application state beforehand.\\n\\n In Gleam we prefer to not use these implicit background process trees, but\\n you will likely still need to start the trees of OTP applications written in\\n other BEAM languages such as Erlang or Elixir, including those included by\\n default with Erlang/OTP.\\n\\n For more information see the OTP documentation.\\n - <https://www.erlang.org/doc/man/application.html#ensure_all_started-1>\\n - <https://www.erlang.org/doc/man/application.html#start-1>\",
        \"nature\": \"function\",
        \"metadata\": {
            \"deprecation\": null,
            \"implementations\": {
                \"gleam\": false,
                \"uses_erlang_externals\": true,
                \"uses_javascript_externals\": false
            }
        },
        \"json_signature\": {
            \"name\": \"ensure_all_started\",
            \"type\": \"function\",
            \"return\": {
                \"ref\": null,
                \"name\": \"Result\",
                \"type\": \"named\",
                \"module\": \"gleam\",
                \"package\": \"\",
                \"parameters\": [
                    {
                        \"ref\": null,
                        \"name\": \"List\",
                        \"type\": \"named\",
                        \"module\": \"gleam\",
                        \"package\": \"\",
                        \"parameters\": [
                            {
                                \"ref\": \"0.25.0\",
                                \"name\": \"Atom\",
                                \"type\": \"named\",
                                \"module\": \"gleam/erlang/atom\",
                                \"package\": \"gleam_erlang\",
                                \"parameters\": []
                            }
                        ]
                    },
                    {
                        \"ref\": \"0.25.0\",
                        \"name\": \"EnsureAllStartedError\",
                        \"type\": \"named\",
                        \"module\": \"gleam/erlang\",
                        \"package\": \"gleam_erlang\",
                        \"parameters\": []
                    }
                ]
            },
            \"parameters\": [
                {
                    \"type\": \"parameter\",
                    \"label\": \"application\",
                    \"params_type\": {
                        \"ref\": \"0.25.0\",
                        \"name\": \"Atom\",
                        \"type\": \"named\",
                        \"module\": \"gleam/erlang/atom\",
                        \"package\": \"gleam_erlang\",
                        \"parameters\": []
                    }
                }
            ],
            \"deprecation\": \" Starts an OTP application's process tree in the background, as well as\\n the trees of any applications that the given application depends upon. An\\n OTP application typically maps onto a Gleam or Hex package.\\n\\n Returns a list of the applications that were started. Calling this function\\n for application that have already been started is a no-op so you do not need\\n to check the application state beforehand.\\n\\n In Gleam we prefer to not use these implicit background process trees, but\\n you will likely still need to start the trees of OTP applications written in\\n other BEAM languages such as Erlang or Elixir, including those included by\\n default with Erlang/OTP.\\n\\n For more information see the OTP documentation.\\n - <https://www.erlang.org/doc/man/application.html#ensure_all_started-1>\\n - <https://www.erlang.org/doc/man/application.html#start-1>\\n\",
            \"documentation\": \" Starts an OTP application's process tree in the background, as well as\\n the trees of any applications that the given application depends upon. An\\n OTP application typically maps onto a Gleam or Hex package.\\n\\n Returns a list of the applications that were started. Calling this function\\n for application that have already been started is a no-op so you do not need\\n to check the application state beforehand.\\n\\n In Gleam we prefer to not use these implicit background process trees, but\\n you will likely still need to start the trees of OTP applications written in\\n other BEAM languages such as Erlang or Elixir, including those included by\\n default with Erlang/OTP.\\n\\n For more information see the OTP documentation.\\n - <https://www.erlang.org/doc/man/application.html#ensure_all_started-1>\\n - <https://www.erlang.org/doc/man/application.html#start-1>\\n\",
            \"implementations\": {
                \"gleam\": false,
                \"uses_erlang_externals\": true,
                \"uses_javascript_externals\": false
            }
        },
        \"module_name\": \"gleam/erlang\",
        \"package_name\": \"gleam_erlang\",
        \"version\": \"0.25.0\"
    },
    {
        \"name\": \"Body\",
        \"documentation\": \"The body of a HTTP response, to be sent to the client.\",
        \"nature\": \"type_definition\",
        \"metadata\": {
            \"deprecation\": null
        },
        \"json_signature\": {
            \"name\": \"Body\",
            \"type\": \"type-definition\",
            \"parameters\": 0,
            \"deprecation\": \" The body of a HTTP response, to be sent to the client.\\n\",
            \"constructors\": [
                {
                    \"name\": \"Text\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": null,
                            \"params_type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"StringBuilder\",
                                \"type\": \"named\",
                                \"module\": \"gleam/string_builder\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": []
                            }
                        }
                    ],
                    \"documentation\": \" A body of unicode text.\\n\\n The body is represented using a `StringBuilder`. If you have a `String`\\n you can use the `string_builder.from_string` function to convert it.\\n\"
                },
                {
                    \"name\": \"Bytes\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": null,
                            \"params_type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"BytesBuilder\",
                                \"type\": \"named\",
                                \"module\": \"gleam/bytes_builder\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": []
                            }
                        }
                    ],
                    \"documentation\": \" A body of binary data.\\n\\n The body is represented using a `StringBuilder`. If you have a `String`\\n you can use the `string_builder.from_string` function to convert it.\\n\"
                },
                {
                    \"name\": \"File\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": \"path\",
                            \"params_type\": {
                                \"ref\": null,
                                \"name\": \"String\",
                                \"type\": \"named\",
                                \"module\": \"gleam\",
                                \"package\": \"\",
                                \"parameters\": []
                            }
                        }
                    ],
                    \"documentation\": \" A body of the contents of a file.\\n\\n This will be sent efficiently using the `send_file` function of the\\n underlying HTTP server. The file will not be read into memory so it is\\n safe to send large files this way.\\n\"
                },
                {
                    \"name\": \"Empty\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [],
                    \"documentation\": \" An empty body. This may be returned by the `require_*` middleware\\n functions in the event of a failure, invalid request, or other situation\\n in which the request cannot be processed.\\n\\n Your application may wish to use a middleware to provide default responses\\n in place of any with an empty body.\\n\"
                }
            ],
            \"documentation\": \" The body of a HTTP response, to be sent to the client.\\n\"
        },
        \"module_name\": \"wisp\",
        \"package_name\": \"wisp\",
        \"version\": \"0.14.0\"
    },
    {
        \"name\": \"Application\",
        \"documentation\": \"Model for a Discord Application: https://discord.com/developers/docs/resources/application\\n Note to self: summary is deprecated and will be removed in v11\",
        \"nature\": \"type_definition\",
        \"metadata\": {
            \"deprecation\": null
        },
        \"json_signature\": {
            \"name\": \"Application\",
            \"type\": \"type-definition\",
            \"parameters\": 0,
            \"deprecation\": \" Model for a Discord Application: https://discord.com/developers/docs/resources/application\\n Note to self: summary is deprecated and will be removed in v11\",
            \"constructors\": [
                {
                    \"name\": \"Application\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": \"id\",
                            \"params_type\": {
                                \"ref\": null,
                                \"name\": \"String\",
                                \"type\": \"named\",
                                \"module\": \"gleam\",
                                \"package\": \"\",
                                \"parameters\": []
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"name\",
                            \"params_type\": {
                                \"ref\": null,
                                \"name\": \"String\",
                                \"type\": \"named\",
                                \"module\": \"gleam\",
                                \"package\": \"\",
                                \"parameters\": []
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"icon\",
                            \"params_type\": {
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
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"description\",
                            \"params_type\": {
                                \"ref\": null,
                                \"name\": \"String\",
                                \"type\": \"named\",
                                \"module\": \"gleam\",
                                \"package\": \"\",
                                \"parameters\": []
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"rpc_origins\",
                            \"params_type\": {
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
                                                \"ref\": null,
                                                \"name\": \"String\",
                                                \"type\": \"named\",
                                                \"module\": \"gleam\",
                                                \"package\": \"\",
                                                \"parameters\": []
                                            }
                                        ]
                                    }
                                ]
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"bot_public\",
                            \"params_type\": {
                                \"ref\": null,
                                \"name\": \"Bool\",
                                \"type\": \"named\",
                                \"module\": \"gleam\",
                                \"package\": \"\",
                                \"parameters\": []
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"bot_require_code_grant\",
                            \"params_type\": {
                                \"ref\": null,
                                \"name\": \"Bool\",
                                \"type\": \"named\",
                                \"module\": \"gleam\",
                                \"package\": \"\",
                                \"parameters\": []
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"bot\",
                            \"params_type\": {
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
                                        \"module\": \"glyph/models/discord\",
                                        \"package\": \"glyph\",
                                        \"parameters\": []
                                    }
                                ]
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"terms_of_service_url\",
                            \"params_type\": {
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
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"privacy_policy_url\",
                            \"params_type\": {
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
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"owner\",
                            \"params_type\": {
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
                                        \"module\": \"glyph/models/discord\",
                                        \"package\": \"glyph\",
                                        \"parameters\": []
                                    }
                                ]
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"summary\",
                            \"params_type\": {
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
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"verify_key\",
                            \"params_type\": {
                                \"ref\": null,
                                \"name\": \"String\",
                                \"type\": \"named\",
                                \"module\": \"gleam\",
                                \"package\": \"\",
                                \"parameters\": []
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"team\",
                            \"params_type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": \"0.1.0\",
                                        \"name\": \"Team\",
                                        \"type\": \"named\",
                                        \"module\": \"glyph/models/discord\",
                                        \"package\": \"glyph\",
                                        \"parameters\": []
                                    }
                                ]
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"guild_id\",
                            \"params_type\": {
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
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"primary_sku_id\",
                            \"params_type\": {
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
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"slug\",
                            \"params_type\": {
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
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"cover_image\",
                            \"params_type\": {
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
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"flags\",
                            \"params_type\": {
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
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"approximate_guild_count\",
                            \"params_type\": {
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
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"redirect_uris\",
                            \"params_type\": {
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
                                                \"ref\": null,
                                                \"name\": \"String\",
                                                \"type\": \"named\",
                                                \"module\": \"gleam\",
                                                \"package\": \"\",
                                                \"parameters\": []
                                            }
                                        ]
                                    }
                                ]
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"interactions_endpoint_url\",
                            \"params_type\": {
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
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"role_connections_verification_url\",
                            \"params_type\": {
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
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"tags\",
                            \"params_type\": {
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
                                                \"ref\": null,
                                                \"name\": \"String\",
                                                \"type\": \"named\",
                                                \"module\": \"gleam\",
                                                \"package\": \"\",
                                                \"parameters\": []
                                            }
                                        ]
                                    }
                                ]
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"install_params\",
                            \"params_type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Option\",
                                \"type\": \"named\",
                                \"module\": \"gleam/option\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": [
                                    {
                                        \"ref\": \"0.1.0\",
                                        \"name\": \"InstallParams\",
                                        \"type\": \"named\",
                                        \"module\": \"glyph/models/discord\",
                                        \"package\": \"glyph\",
                                        \"parameters\": []
                                    }
                                ]
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"custom_install_url\",
                            \"params_type\": {
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
                            }
                        }
                    ],
                    \"documentation\": null
                }
            ],
            \"documentation\": \" Model for a Discord Application: https://discord.com/developers/docs/resources/application\\n Note to self: summary is deprecated and will be removed in v11\"
        },
        \"module_name\": \"glyph/models/discord\",
        \"package_name\": \"glyph\",
        \"version\": \"0.1.0\"
    },
    {
        \"name\": \"application_stopped\",
        \"documentation\": \"\",
        \"nature\": \"function\",
        \"metadata\": {
            \"deprecation\": null,
            \"implementations\": {
                \"gleam\": false,
                \"uses_erlang_externals\": true,
                \"uses_javascript_externals\": false
            }
        },
        \"json_signature\": {
            \"name\": \"application_stopped\",
            \"type\": \"function\",
            \"return\": {
                \"ref\": \"0.9.0\",
                \"name\": \"ApplicationStop\",
                \"type\": \"named\",
                \"module\": \"gleam/otp/supervisor\",
                \"package\": \"gleam_otp\",
                \"parameters\": []
            },
            \"parameters\": [],
            \"deprecation\": null,
            \"documentation\": null,
            \"implementations\": {
                \"gleam\": false,
                \"uses_erlang_externals\": true,
                \"uses_javascript_externals\": false
            }
        },
        \"module_name\": \"gleam/otp/supervisor\",
        \"package_name\": \"gleam_otp\",
        \"version\": \"0.9.0\"
    },
    {
        \"name\": \"application\",
        \"documentation\": \"A complete Lustre application that follows the Model-View-Update architecture\\n and can handle side effects like HTTP requests or querying the DOM. Most real\\n Lustre applications will use this constructor.\\n\\n To learn more about effects and their purpose, take a look at the\\n [`effect`](./lustre/effect.html) module or the\\n [HTTP requests example](https://github.com/lustre-labs/lustre/tree/main/examples/05-http-requests).\",
        \"nature\": \"function\",
        \"metadata\": {
            \"deprecation\": null,
            \"implementations\": {
                \"gleam\": true,
                \"uses_erlang_externals\": false,
                \"uses_javascript_externals\": false
            }
        },
        \"json_signature\": {
            \"name\": \"application\",
            \"type\": \"function\",
            \"return\": {
                \"ref\": \"4.2.0\",
                \"name\": \"App\",
                \"type\": \"named\",
                \"module\": \"lustre\",
                \"package\": \"lustre\",
                \"parameters\": [
                    {
                        \"id\": 0,
                        \"type\": \"variable\"
                    },
                    {
                        \"id\": 1,
                        \"type\": \"variable\"
                    },
                    {
                        \"id\": 2,
                        \"type\": \"variable\"
                    }
                ]
            },
            \"parameters\": [
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
                        \"type\": \"fn\",
                        \"params\": [
                            {
                                \"id\": 0,
                                \"type\": \"variable\"
                            }
                        ],
                        \"return\": {
                            \"type\": \"tuple\",
                            \"elements\": [
                                {
                                    \"id\": 1,
                                    \"type\": \"variable\"
                                },
                                {
                                    \"ref\": \"4.2.0\",
                                    \"name\": \"Effect\",
                                    \"type\": \"named\",
                                    \"module\": \"lustre/effect\",
                                    \"package\": \"lustre\",
                                    \"parameters\": [
                                        {
                                            \"id\": 2,
                                            \"type\": \"variable\"
                                        }
                                    ]
                                }
                            ]
                        }
                    }
                },
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
                        \"type\": \"fn\",
                        \"params\": [
                            {
                                \"id\": 1,
                                \"type\": \"variable\"
                            },
                            {
                                \"id\": 2,
                                \"type\": \"variable\"
                            }
                        ],
                        \"return\": {
                            \"type\": \"tuple\",
                            \"elements\": [
                                {
                                    \"id\": 1,
                                    \"type\": \"variable\"
                                },
                                {
                                    \"ref\": \"4.2.0\",
                                    \"name\": \"Effect\",
                                    \"type\": \"named\",
                                    \"module\": \"lustre/effect\",
                                    \"package\": \"lustre\",
                                    \"parameters\": [
                                        {
                                            \"id\": 2,
                                            \"type\": \"variable\"
                                        }
                                    ]
                                }
                            ]
                        }
                    }
                },
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
                        \"type\": \"fn\",
                        \"params\": [
                            {
                                \"id\": 1,
                                \"type\": \"variable\"
                            }
                        ],
                        \"return\": {
                            \"ref\": null,
                            \"name\": \"Element\",
                            \"type\": \"named\",
                            \"module\": \"lustre/internals/vdom\",
                            \"package\": \"lustre\",
                            \"parameters\": [
                                {
                                    \"id\": 2,
                                    \"type\": \"variable\"
                                }
                            ]
                        }
                    }
                }
            ],
            \"deprecation\": \" A complete Lustre application that follows the Model-View-Update architecture\\n and can handle side effects like HTTP requests or querying the DOM. Most real\\n Lustre applications will use this constructor.\\n\\n To learn more about effects and their purpose, take a look at the\\n [`effect`](./lustre/effect.html) module or the\\n [HTTP requests example](https://github.com/lustre-labs/lustre/tree/main/examples/05-http-requests).\\n\",
            \"documentation\": \" A complete Lustre application that follows the Model-View-Update architecture\\n and can handle side effects like HTTP requests or querying the DOM. Most real\\n Lustre applications will use this constructor.\\n\\n To learn more about effects and their purpose, take a look at the\\n [`effect`](./lustre/effect.html) module or the\\n [HTTP requests example](https://github.com/lustre-labs/lustre/tree/main/examples/05-http-requests).\\n\",
            \"implementations\": {
                \"gleam\": true,
                \"uses_erlang_externals\": false,
                \"uses_javascript_externals\": false
            }
        },
        \"module_name\": \"lustre\",
        \"package_name\": \"lustre\",
        \"version\": \"4.2.0\"
    },
    {
        \"name\": \"ContentType\",
        \"documentation\": \"\",
        \"nature\": \"type_definition\",
        \"metadata\": {
            \"deprecation\": null
        },
        \"json_signature\": {
            \"name\": \"ContentType\",
            \"type\": \"type-definition\",
            \"parameters\": 0,
            \"deprecation\": null,
            \"constructors\": [
                {
                    \"name\": \"Application\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": null,
                            \"params_type\": {
                                \"ref\": \"1.1.1\",
                                \"name\": \"Application\",
                                \"type\": \"named\",
                                \"module\": \"typed_headers/content_type/application\",
                                \"package\": \"typed_headers\",
                                \"parameters\": []
                            }
                        }
                    ],
                    \"documentation\": null
                },
                {
                    \"name\": \"ApplicationWithCodecs\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": null,
                            \"params_type\": {
                                \"ref\": \"1.1.1\",
                                \"name\": \"Application\",
                                \"type\": \"named\",
                                \"module\": \"typed_headers/content_type/application\",
                                \"package\": \"typed_headers\",
                                \"parameters\": []
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"codecs\",
                            \"params_type\": {
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
                            }
                        }
                    ],
                    \"documentation\": null
                },
                {
                    \"name\": \"Audio\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": null,
                            \"params_type\": {
                                \"ref\": \"1.1.1\",
                                \"name\": \"Audio\",
                                \"type\": \"named\",
                                \"module\": \"typed_headers/content_type/audio\",
                                \"package\": \"typed_headers\",
                                \"parameters\": []
                            }
                        }
                    ],
                    \"documentation\": null
                },
                {
                    \"name\": \"AudioWithCodecs\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": null,
                            \"params_type\": {
                                \"ref\": \"1.1.1\",
                                \"name\": \"Audio\",
                                \"type\": \"named\",
                                \"module\": \"typed_headers/content_type/audio\",
                                \"package\": \"typed_headers\",
                                \"parameters\": []
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"codecs\",
                            \"params_type\": {
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
                            }
                        }
                    ],
                    \"documentation\": null
                },
                {
                    \"name\": \"Font\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": null,
                            \"params_type\": {
                                \"ref\": \"1.1.1\",
                                \"name\": \"Font\",
                                \"type\": \"named\",
                                \"module\": \"typed_headers/content_type/font\",
                                \"package\": \"typed_headers\",
                                \"parameters\": []
                            }
                        }
                    ],
                    \"documentation\": null
                },
                {
                    \"name\": \"Example\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": null,
                            \"params_type\": {
                                \"ref\": \"1.1.1\",
                                \"name\": \"Example\",
                                \"type\": \"named\",
                                \"module\": \"typed_headers/content_type/example\",
                                \"package\": \"typed_headers\",
                                \"parameters\": []
                            }
                        }
                    ],
                    \"documentation\": null
                },
                {
                    \"name\": \"Image\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": null,
                            \"params_type\": {
                                \"ref\": \"1.1.1\",
                                \"name\": \"Image\",
                                \"type\": \"named\",
                                \"module\": \"typed_headers/content_type/image\",
                                \"package\": \"typed_headers\",
                                \"parameters\": []
                            }
                        }
                    ],
                    \"documentation\": null
                },
                {
                    \"name\": \"Message\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": null,
                            \"params_type\": {
                                \"ref\": \"1.1.1\",
                                \"name\": \"Message\",
                                \"type\": \"named\",
                                \"module\": \"typed_headers/content_type/message\",
                                \"package\": \"typed_headers\",
                                \"parameters\": []
                            }
                        }
                    ],
                    \"documentation\": null
                },
                {
                    \"name\": \"Model\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": null,
                            \"params_type\": {
                                \"ref\": \"1.1.1\",
                                \"name\": \"Model\",
                                \"type\": \"named\",
                                \"module\": \"typed_headers/content_type/model\",
                                \"package\": \"typed_headers\",
                                \"parameters\": []
                            }
                        }
                    ],
                    \"documentation\": null
                },
                {
                    \"name\": \"Multipart\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": null,
                            \"params_type\": {
                                \"ref\": \"1.1.1\",
                                \"name\": \"Multipart\",
                                \"type\": \"named\",
                                \"module\": \"typed_headers/content_type/multipart\",
                                \"package\": \"typed_headers\",
                                \"parameters\": []
                            }
                        }
                    ],
                    \"documentation\": null
                },
                {
                    \"name\": \"Text\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": null,
                            \"params_type\": {
                                \"ref\": \"1.1.1\",
                                \"name\": \"Text\",
                                \"type\": \"named\",
                                \"module\": \"typed_headers/content_type/text\",
                                \"package\": \"typed_headers\",
                                \"parameters\": []
                            }
                        }
                    ],
                    \"documentation\": null
                },
                {
                    \"name\": \"Video\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": null,
                            \"params_type\": {
                                \"ref\": \"1.1.1\",
                                \"name\": \"Video\",
                                \"type\": \"named\",
                                \"module\": \"typed_headers/content_type/video\",
                                \"package\": \"typed_headers\",
                                \"parameters\": []
                            }
                        }
                    ],
                    \"documentation\": null
                },
                {
                    \"name\": \"VideoWithCodecs\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": null,
                            \"params_type\": {
                                \"ref\": \"1.1.1\",
                                \"name\": \"Video\",
                                \"type\": \"named\",
                                \"module\": \"typed_headers/content_type/video\",
                                \"package\": \"typed_headers\",
                                \"parameters\": []
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"codecs\",
                            \"params_type\": {
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
                            }
                        }
                    ],
                    \"documentation\": null
                },
                {
                    \"name\": \"Custom\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": \"media_type\",
                            \"params_type\": {
                                \"ref\": null,
                                \"name\": \"String\",
                                \"type\": \"named\",
                                \"module\": \"gleam\",
                                \"package\": \"\",
                                \"parameters\": []
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"sub_type\",
                            \"params_type\": {
                                \"ref\": null,
                                \"name\": \"String\",
                                \"type\": \"named\",
                                \"module\": \"gleam\",
                                \"package\": \"\",
                                \"parameters\": []
                            }
                        }
                    ],
                    \"documentation\": null
                },
                {
                    \"name\": \"Raw\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": null,
                            \"params_type\": {
                                \"ref\": null,
                                \"name\": \"String\",
                                \"type\": \"named\",
                                \"module\": \"gleam\",
                                \"package\": \"\",
                                \"parameters\": []
                            }
                        }
                    ],
                    \"documentation\": null
                }
            ],
            \"documentation\": null
        },
        \"module_name\": \"typed_headers/content_type\",
        \"package_name\": \"typed_headers\",
        \"version\": \"1.1.1\"
    },
    {
        \"name\": \"Application\",
        \"documentation\": \"\",
        \"nature\": \"type_definition\",
        \"metadata\": {
            \"deprecation\": null
        },
        \"json_signature\": {
            \"name\": \"Application\",
            \"type\": \"type-definition\",
            \"parameters\": 0,
            \"deprecation\": null,
            \"constructors\": [
                {
                    \"name\": \"JSON\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [],
                    \"documentation\": null
                },
                {
                    \"name\": \"XML\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [],
                    \"documentation\": null
                },
                {
                    \"name\": \"OGG\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [],
                    \"documentation\": null
                },
                {
                    \"name\": \"OGGWithCodecs\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": \"codecs\",
                            \"params_type\": {
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
                            }
                        }
                    ],
                    \"documentation\": null
                },
                {
                    \"name\": \"Custom\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": null,
                            \"params_type\": {
                                \"ref\": null,
                                \"name\": \"String\",
                                \"type\": \"named\",
                                \"module\": \"gleam\",
                                \"package\": \"\",
                                \"parameters\": []
                            }
                        }
                    ],
                    \"documentation\": null
                }
            ],
            \"documentation\": null
        },
        \"module_name\": \"typed_headers/content_type/application\",
        \"package_name\": \"typed_headers\",
        \"version\": \"1.1.1\"
    },
    {
        \"name\": \"application\",
        \"documentation\": \"A complete Lustre application that follows the Model-View-Update architecture\\n and can handle side effects like HTTP requests or querying the DOM. Most real\\n Lustre applications will use this constructor.\\n\\n To learn more about effects and their purpose, take a look at the\\n [`effect`](./lustre/effect) module or the\\n [HTTP requests example](https://github.com/lustre-labs/lustre/tree/main/examples/05-http-requests).\",
        \"nature\": \"function\",
        \"metadata\": {
            \"deprecation\": null,
            \"implementations\": {
                \"gleam\": true,
                \"uses_erlang_externals\": false,
                \"uses_javascript_externals\": false
            }
        },
        \"json_signature\": {
            \"name\": \"application\",
            \"type\": \"function\",
            \"return\": {
                \"ref\": \"4.1.0\",
                \"name\": \"App\",
                \"type\": \"named\",
                \"module\": \"lustre\",
                \"package\": \"lustre\",
                \"parameters\": [
                    {
                        \"id\": 0,
                        \"type\": \"variable\"
                    },
                    {
                        \"id\": 1,
                        \"type\": \"variable\"
                    },
                    {
                        \"id\": 2,
                        \"type\": \"variable\"
                    }
                ]
            },
            \"parameters\": [
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
                        \"type\": \"fn\",
                        \"params\": [
                            {
                                \"id\": 0,
                                \"type\": \"variable\"
                            }
                        ],
                        \"return\": {
                            \"type\": \"tuple\",
                            \"elements\": [
                                {
                                    \"id\": 1,
                                    \"type\": \"variable\"
                                },
                                {
                                    \"ref\": \"4.1.0\",
                                    \"name\": \"Effect\",
                                    \"type\": \"named\",
                                    \"module\": \"lustre/effect\",
                                    \"package\": \"lustre\",
                                    \"parameters\": [
                                        {
                                            \"id\": 2,
                                            \"type\": \"variable\"
                                        }
                                    ]
                                }
                            ]
                        }
                    }
                },
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
                        \"type\": \"fn\",
                        \"params\": [
                            {
                                \"id\": 1,
                                \"type\": \"variable\"
                            },
                            {
                                \"id\": 2,
                                \"type\": \"variable\"
                            }
                        ],
                        \"return\": {
                            \"type\": \"tuple\",
                            \"elements\": [
                                {
                                    \"id\": 1,
                                    \"type\": \"variable\"
                                },
                                {
                                    \"ref\": \"4.1.0\",
                                    \"name\": \"Effect\",
                                    \"type\": \"named\",
                                    \"module\": \"lustre/effect\",
                                    \"package\": \"lustre\",
                                    \"parameters\": [
                                        {
                                            \"id\": 2,
                                            \"type\": \"variable\"
                                        }
                                    ]
                                }
                            ]
                        }
                    }
                },
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
                        \"type\": \"fn\",
                        \"params\": [
                            {
                                \"id\": 1,
                                \"type\": \"variable\"
                            }
                        ],
                        \"return\": {
                            \"ref\": null,
                            \"name\": \"Element\",
                            \"type\": \"named\",
                            \"module\": \"lustre/internals/vdom\",
                            \"package\": \"lustre\",
                            \"parameters\": [
                                {
                                    \"id\": 2,
                                    \"type\": \"variable\"
                                }
                            ]
                        }
                    }
                }
            ],
            \"deprecation\": \" A complete Lustre application that follows the Model-View-Update architecture\\n and can handle side effects like HTTP requests or querying the DOM. Most real\\n Lustre applications will use this constructor.\\n\\n To learn more about effects and their purpose, take a look at the\\n [`effect`](./lustre/effect) module or the\\n [HTTP requests example](https://github.com/lustre-labs/lustre/tree/main/examples/05-http-requests).\\n\",
            \"documentation\": \" A complete Lustre application that follows the Model-View-Update architecture\\n and can handle side effects like HTTP requests or querying the DOM. Most real\\n Lustre applications will use this constructor.\\n\\n To learn more about effects and their purpose, take a look at the\\n [`effect`](./lustre/effect) module or the\\n [HTTP requests example](https://github.com/lustre-labs/lustre/tree/main/examples/05-http-requests).\\n\",
            \"implementations\": {
                \"gleam\": true,
                \"uses_erlang_externals\": false,
                \"uses_javascript_externals\": false
            }
        },
        \"module_name\": \"lustre\",
        \"package_name\": \"lustre\",
        \"version\": \"4.1.0\"
    },
    {
        \"name\": \"application\",
        \"documentation\": \"A complete Lustre application that follows the Model-View-Update architecture\\n and can handle side effects like HTTP requests or querying the DOM. Most real\\n Lustre applications will use this constructor.\\n\\n To learn more about effects and their purpose, take a look at the\\n [`effect`](./lustre/effect) module or the\\n [HTTP requests example](https://github.com/lustre-labs/lustre/tree/main/examples/05-http-requests).\",
        \"nature\": \"function\",
        \"metadata\": {
            \"deprecation\": null,
            \"implementations\": {
                \"gleam\": true,
                \"uses_erlang_externals\": false,
                \"uses_javascript_externals\": false
            }
        },
        \"json_signature\": {
            \"name\": \"application\",
            \"type\": \"function\",
            \"return\": {
                \"ref\": \"4.1.4\",
                \"name\": \"App\",
                \"type\": \"named\",
                \"module\": \"lustre\",
                \"package\": \"lustre\",
                \"parameters\": [
                    {
                        \"id\": 0,
                        \"type\": \"variable\"
                    },
                    {
                        \"id\": 1,
                        \"type\": \"variable\"
                    },
                    {
                        \"id\": 2,
                        \"type\": \"variable\"
                    }
                ]
            },
            \"parameters\": [
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
                        \"type\": \"fn\",
                        \"params\": [
                            {
                                \"id\": 0,
                                \"type\": \"variable\"
                            }
                        ],
                        \"return\": {
                            \"type\": \"tuple\",
                            \"elements\": [
                                {
                                    \"id\": 1,
                                    \"type\": \"variable\"
                                },
                                {
                                    \"ref\": \"4.1.4\",
                                    \"name\": \"Effect\",
                                    \"type\": \"named\",
                                    \"module\": \"lustre/effect\",
                                    \"package\": \"lustre\",
                                    \"parameters\": [
                                        {
                                            \"id\": 2,
                                            \"type\": \"variable\"
                                        }
                                    ]
                                }
                            ]
                        }
                    }
                },
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
                        \"type\": \"fn\",
                        \"params\": [
                            {
                                \"id\": 1,
                                \"type\": \"variable\"
                            },
                            {
                                \"id\": 2,
                                \"type\": \"variable\"
                            }
                        ],
                        \"return\": {
                            \"type\": \"tuple\",
                            \"elements\": [
                                {
                                    \"id\": 1,
                                    \"type\": \"variable\"
                                },
                                {
                                    \"ref\": \"4.1.4\",
                                    \"name\": \"Effect\",
                                    \"type\": \"named\",
                                    \"module\": \"lustre/effect\",
                                    \"package\": \"lustre\",
                                    \"parameters\": [
                                        {
                                            \"id\": 2,
                                            \"type\": \"variable\"
                                        }
                                    ]
                                }
                            ]
                        }
                    }
                },
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
                        \"type\": \"fn\",
                        \"params\": [
                            {
                                \"id\": 1,
                                \"type\": \"variable\"
                            }
                        ],
                        \"return\": {
                            \"ref\": null,
                            \"name\": \"Element\",
                            \"type\": \"named\",
                            \"module\": \"lustre/internals/vdom\",
                            \"package\": \"lustre\",
                            \"parameters\": [
                                {
                                    \"id\": 2,
                                    \"type\": \"variable\"
                                }
                            ]
                        }
                    }
                }
            ],
            \"deprecation\": \" A complete Lustre application that follows the Model-View-Update architecture\\n and can handle side effects like HTTP requests or querying the DOM. Most real\\n Lustre applications will use this constructor.\\n\\n To learn more about effects and their purpose, take a look at the\\n [`effect`](./lustre/effect) module or the\\n [HTTP requests example](https://github.com/lustre-labs/lustre/tree/main/examples/05-http-requests).\\n\",
            \"documentation\": \" A complete Lustre application that follows the Model-View-Update architecture\\n and can handle side effects like HTTP requests or querying the DOM. Most real\\n Lustre applications will use this constructor.\\n\\n To learn more about effects and their purpose, take a look at the\\n [`effect`](./lustre/effect) module or the\\n [HTTP requests example](https://github.com/lustre-labs/lustre/tree/main/examples/05-http-requests).\\n\",
            \"implementations\": {
                \"gleam\": true,
                \"uses_erlang_externals\": false,
                \"uses_javascript_externals\": false
            }
        },
        \"module_name\": \"lustre\",
        \"package_name\": \"lustre\",
        \"version\": \"4.1.4\"
    },
    {
        \"name\": \"application\",
        \"documentation\": \"A complete Lustre application that follows the Model-View-Update architecture\\n and can handle side effects like HTTP requests or querying the DOM. Most real\\n Lustre applications will use this constructor.\\n\\n To learn more about effects and their purpose, take a look at the\\n [`effect`](./lustre/effect) module or the\\n [HTTP requests example](https://github.com/lustre-labs/lustre/tree/main/examples/05-http-requests).\",
        \"nature\": \"function\",
        \"metadata\": {
            \"deprecation\": null,
            \"implementations\": {
                \"gleam\": true,
                \"uses_erlang_externals\": false,
                \"uses_javascript_externals\": false
            }
        },
        \"json_signature\": {
            \"name\": \"application\",
            \"type\": \"function\",
            \"return\": {
                \"ref\": \"4.1.7\",
                \"name\": \"App\",
                \"type\": \"named\",
                \"module\": \"lustre\",
                \"package\": \"lustre\",
                \"parameters\": [
                    {
                        \"id\": 0,
                        \"type\": \"variable\"
                    },
                    {
                        \"id\": 1,
                        \"type\": \"variable\"
                    },
                    {
                        \"id\": 2,
                        \"type\": \"variable\"
                    }
                ]
            },
            \"parameters\": [
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
                        \"type\": \"fn\",
                        \"params\": [
                            {
                                \"id\": 0,
                                \"type\": \"variable\"
                            }
                        ],
                        \"return\": {
                            \"type\": \"tuple\",
                            \"elements\": [
                                {
                                    \"id\": 1,
                                    \"type\": \"variable\"
                                },
                                {
                                    \"ref\": \"4.1.7\",
                                    \"name\": \"Effect\",
                                    \"type\": \"named\",
                                    \"module\": \"lustre/effect\",
                                    \"package\": \"lustre\",
                                    \"parameters\": [
                                        {
                                            \"id\": 2,
                                            \"type\": \"variable\"
                                        }
                                    ]
                                }
                            ]
                        }
                    }
                },
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
                        \"type\": \"fn\",
                        \"params\": [
                            {
                                \"id\": 1,
                                \"type\": \"variable\"
                            },
                            {
                                \"id\": 2,
                                \"type\": \"variable\"
                            }
                        ],
                        \"return\": {
                            \"type\": \"tuple\",
                            \"elements\": [
                                {
                                    \"id\": 1,
                                    \"type\": \"variable\"
                                },
                                {
                                    \"ref\": \"4.1.7\",
                                    \"name\": \"Effect\",
                                    \"type\": \"named\",
                                    \"module\": \"lustre/effect\",
                                    \"package\": \"lustre\",
                                    \"parameters\": [
                                        {
                                            \"id\": 2,
                                            \"type\": \"variable\"
                                        }
                                    ]
                                }
                            ]
                        }
                    }
                },
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
                        \"type\": \"fn\",
                        \"params\": [
                            {
                                \"id\": 1,
                                \"type\": \"variable\"
                            }
                        ],
                        \"return\": {
                            \"ref\": null,
                            \"name\": \"Element\",
                            \"type\": \"named\",
                            \"module\": \"lustre/internals/vdom\",
                            \"package\": \"lustre\",
                            \"parameters\": [
                                {
                                    \"id\": 2,
                                    \"type\": \"variable\"
                                }
                            ]
                        }
                    }
                }
            ],
            \"deprecation\": \" A complete Lustre application that follows the Model-View-Update architecture\\n and can handle side effects like HTTP requests or querying the DOM. Most real\\n Lustre applications will use this constructor.\\n\\n To learn more about effects and their purpose, take a look at the\\n [`effect`](./lustre/effect) module or the\\n [HTTP requests example](https://github.com/lustre-labs/lustre/tree/main/examples/05-http-requests).\\n\",
            \"documentation\": \" A complete Lustre application that follows the Model-View-Update architecture\\n and can handle side effects like HTTP requests or querying the DOM. Most real\\n Lustre applications will use this constructor.\\n\\n To learn more about effects and their purpose, take a look at the\\n [`effect`](./lustre/effect) module or the\\n [HTTP requests example](https://github.com/lustre-labs/lustre/tree/main/examples/05-http-requests).\\n\",
            \"implementations\": {
                \"gleam\": true,
                \"uses_erlang_externals\": false,
                \"uses_javascript_externals\": false
            }
        },
        \"module_name\": \"lustre\",
        \"package_name\": \"lustre\",
        \"version\": \"4.1.7\"
    },
    {
        \"name\": \"application\",
        \"documentation\": \"A complete Lustre application that follows the Model-View-Update architecture\\n and can handle side effects like HTTP requests or querying the DOM. Most real\\n Lustre applications will use this constructor.\\n\\n To learn more about effects and their purpose, take a look at the\\n [`effect`](./lustre/effect) module or the\\n [HTTP requests example](https://github.com/lustre-labs/lustre/tree/main/examples/05-http-requests).\",
        \"nature\": \"function\",
        \"metadata\": {
            \"deprecation\": null,
            \"implementations\": {
                \"gleam\": true,
                \"uses_erlang_externals\": false,
                \"uses_javascript_externals\": false
            }
        },
        \"json_signature\": {
            \"name\": \"application\",
            \"type\": \"function\",
            \"return\": {
                \"ref\": \"4.1.6\",
                \"name\": \"App\",
                \"type\": \"named\",
                \"module\": \"lustre\",
                \"package\": \"lustre\",
                \"parameters\": [
                    {
                        \"id\": 0,
                        \"type\": \"variable\"
                    },
                    {
                        \"id\": 1,
                        \"type\": \"variable\"
                    },
                    {
                        \"id\": 2,
                        \"type\": \"variable\"
                    }
                ]
            },
            \"parameters\": [
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
                        \"type\": \"fn\",
                        \"params\": [
                            {
                                \"id\": 0,
                                \"type\": \"variable\"
                            }
                        ],
                        \"return\": {
                            \"type\": \"tuple\",
                            \"elements\": [
                                {
                                    \"id\": 1,
                                    \"type\": \"variable\"
                                },
                                {
                                    \"ref\": \"4.1.6\",
                                    \"name\": \"Effect\",
                                    \"type\": \"named\",
                                    \"module\": \"lustre/effect\",
                                    \"package\": \"lustre\",
                                    \"parameters\": [
                                        {
                                            \"id\": 2,
                                            \"type\": \"variable\"
                                        }
                                    ]
                                }
                            ]
                        }
                    }
                },
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
                        \"type\": \"fn\",
                        \"params\": [
                            {
                                \"id\": 1,
                                \"type\": \"variable\"
                            },
                            {
                                \"id\": 2,
                                \"type\": \"variable\"
                            }
                        ],
                        \"return\": {
                            \"type\": \"tuple\",
                            \"elements\": [
                                {
                                    \"id\": 1,
                                    \"type\": \"variable\"
                                },
                                {
                                    \"ref\": \"4.1.6\",
                                    \"name\": \"Effect\",
                                    \"type\": \"named\",
                                    \"module\": \"lustre/effect\",
                                    \"package\": \"lustre\",
                                    \"parameters\": [
                                        {
                                            \"id\": 2,
                                            \"type\": \"variable\"
                                        }
                                    ]
                                }
                            ]
                        }
                    }
                },
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
                        \"type\": \"fn\",
                        \"params\": [
                            {
                                \"id\": 1,
                                \"type\": \"variable\"
                            }
                        ],
                        \"return\": {
                            \"ref\": null,
                            \"name\": \"Element\",
                            \"type\": \"named\",
                            \"module\": \"lustre/internals/vdom\",
                            \"package\": \"lustre\",
                            \"parameters\": [
                                {
                                    \"id\": 2,
                                    \"type\": \"variable\"
                                }
                            ]
                        }
                    }
                }
            ],
            \"deprecation\": \" A complete Lustre application that follows the Model-View-Update architecture\\n and can handle side effects like HTTP requests or querying the DOM. Most real\\n Lustre applications will use this constructor.\\n\\n To learn more about effects and their purpose, take a look at the\\n [`effect`](./lustre/effect) module or the\\n [HTTP requests example](https://github.com/lustre-labs/lustre/tree/main/examples/05-http-requests).\\n\",
            \"documentation\": \" A complete Lustre application that follows the Model-View-Update architecture\\n and can handle side effects like HTTP requests or querying the DOM. Most real\\n Lustre applications will use this constructor.\\n\\n To learn more about effects and their purpose, take a look at the\\n [`effect`](./lustre/effect) module or the\\n [HTTP requests example](https://github.com/lustre-labs/lustre/tree/main/examples/05-http-requests).\\n\",
            \"implementations\": {
                \"gleam\": true,
                \"uses_erlang_externals\": false,
                \"uses_javascript_externals\": false
            }
        },
        \"module_name\": \"lustre\",
        \"package_name\": \"lustre\",
        \"version\": \"4.1.6\"
    },
    {
        \"name\": \"Body\",
        \"documentation\": \"The body of a HTTP response, to be sent to the client.\",
        \"nature\": \"type_definition\",
        \"metadata\": {
            \"deprecation\": null
        },
        \"json_signature\": {
            \"name\": \"Body\",
            \"type\": \"type-definition\",
            \"parameters\": 0,
            \"deprecation\": \" The body of a HTTP response, to be sent to the client.\\n\",
            \"constructors\": [
                {
                    \"name\": \"Text\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": null,
                            \"params_type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"StringBuilder\",
                                \"type\": \"named\",
                                \"module\": \"gleam/string_builder\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": []
                            }
                        }
                    ],
                    \"documentation\": \" A body of unicode text.\\n\\n The body is represented using a `StringBuilder`. If you have a `String`\\n you can use the `string_builder.from_string` function to convert it.\\n\"
                },
                {
                    \"name\": \"File\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": \"path\",
                            \"params_type\": {
                                \"ref\": null,
                                \"name\": \"String\",
                                \"type\": \"named\",
                                \"module\": \"gleam\",
                                \"package\": \"\",
                                \"parameters\": []
                            }
                        }
                    ],
                    \"documentation\": \" A body of the contents of a file.\\n\\n This will be sent efficiently using the `send_file` function of the\\n underlying HTTP server. The file will not be read into memory so it is\\n safe to send large files this way.\\n\"
                },
                {
                    \"name\": \"Empty\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [],
                    \"documentation\": \" An empty body. This may be returned by the `require_*` middleware\\n functions in the event of a failure, invalid request, or other situation\\n in which the request cannot be processed.\\n\\n Your application may wish to use a middleware to provide default responses\\n in place of any with an empty body.\\n\"
                }
            ],
            \"documentation\": \" The body of a HTTP response, to be sent to the client.\\n\"
        },
        \"module_name\": \"wisp\",
        \"package_name\": \"wisp\",
        \"version\": \"0.9.0\"
    },
    {
        \"name\": \"ContentType\",
        \"documentation\": \"\",
        \"nature\": \"type_definition\",
        \"metadata\": {
            \"deprecation\": null
        },
        \"json_signature\": {
            \"name\": \"ContentType\",
            \"type\": \"type-definition\",
            \"parameters\": 0,
            \"deprecation\": null,
            \"constructors\": [
                {
                    \"name\": \"Application\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": null,
                            \"params_type\": {
                                \"ref\": \"1.1.0\",
                                \"name\": \"Application\",
                                \"type\": \"named\",
                                \"module\": \"typed_headers/content_type/application\",
                                \"package\": \"typed_headers\",
                                \"parameters\": []
                            }
                        }
                    ],
                    \"documentation\": null
                },
                {
                    \"name\": \"ApplicationWithCodecs\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": null,
                            \"params_type\": {
                                \"ref\": \"1.1.0\",
                                \"name\": \"Application\",
                                \"type\": \"named\",
                                \"module\": \"typed_headers/content_type/application\",
                                \"package\": \"typed_headers\",
                                \"parameters\": []
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"codecs\",
                            \"params_type\": {
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
                            }
                        }
                    ],
                    \"documentation\": null
                },
                {
                    \"name\": \"Audio\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": null,
                            \"params_type\": {
                                \"ref\": \"1.1.0\",
                                \"name\": \"Audio\",
                                \"type\": \"named\",
                                \"module\": \"typed_headers/content_type/audio\",
                                \"package\": \"typed_headers\",
                                \"parameters\": []
                            }
                        }
                    ],
                    \"documentation\": null
                },
                {
                    \"name\": \"AudioWithCodecs\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": null,
                            \"params_type\": {
                                \"ref\": \"1.1.0\",
                                \"name\": \"Audio\",
                                \"type\": \"named\",
                                \"module\": \"typed_headers/content_type/audio\",
                                \"package\": \"typed_headers\",
                                \"parameters\": []
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"codecs\",
                            \"params_type\": {
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
                            }
                        }
                    ],
                    \"documentation\": null
                },
                {
                    \"name\": \"Font\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": null,
                            \"params_type\": {
                                \"ref\": \"1.1.0\",
                                \"name\": \"Font\",
                                \"type\": \"named\",
                                \"module\": \"typed_headers/content_type/font\",
                                \"package\": \"typed_headers\",
                                \"parameters\": []
                            }
                        }
                    ],
                    \"documentation\": null
                },
                {
                    \"name\": \"Example\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": null,
                            \"params_type\": {
                                \"ref\": \"1.1.0\",
                                \"name\": \"Example\",
                                \"type\": \"named\",
                                \"module\": \"typed_headers/content_type/example\",
                                \"package\": \"typed_headers\",
                                \"parameters\": []
                            }
                        }
                    ],
                    \"documentation\": null
                },
                {
                    \"name\": \"Image\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": null,
                            \"params_type\": {
                                \"ref\": \"1.1.0\",
                                \"name\": \"Image\",
                                \"type\": \"named\",
                                \"module\": \"typed_headers/content_type/image\",
                                \"package\": \"typed_headers\",
                                \"parameters\": []
                            }
                        }
                    ],
                    \"documentation\": null
                },
                {
                    \"name\": \"Message\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": null,
                            \"params_type\": {
                                \"ref\": \"1.1.0\",
                                \"name\": \"Message\",
                                \"type\": \"named\",
                                \"module\": \"typed_headers/content_type/message\",
                                \"package\": \"typed_headers\",
                                \"parameters\": []
                            }
                        }
                    ],
                    \"documentation\": null
                },
                {
                    \"name\": \"Model\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": null,
                            \"params_type\": {
                                \"ref\": \"1.1.0\",
                                \"name\": \"Model\",
                                \"type\": \"named\",
                                \"module\": \"typed_headers/content_type/model\",
                                \"package\": \"typed_headers\",
                                \"parameters\": []
                            }
                        }
                    ],
                    \"documentation\": null
                },
                {
                    \"name\": \"Multipart\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": null,
                            \"params_type\": {
                                \"ref\": \"1.1.0\",
                                \"name\": \"Multipart\",
                                \"type\": \"named\",
                                \"module\": \"typed_headers/content_type/multipart\",
                                \"package\": \"typed_headers\",
                                \"parameters\": []
                            }
                        }
                    ],
                    \"documentation\": null
                },
                {
                    \"name\": \"Text\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": null,
                            \"params_type\": {
                                \"ref\": \"1.1.0\",
                                \"name\": \"Text\",
                                \"type\": \"named\",
                                \"module\": \"typed_headers/content_type/text\",
                                \"package\": \"typed_headers\",
                                \"parameters\": []
                            }
                        }
                    ],
                    \"documentation\": null
                },
                {
                    \"name\": \"Video\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": null,
                            \"params_type\": {
                                \"ref\": \"1.1.0\",
                                \"name\": \"Video\",
                                \"type\": \"named\",
                                \"module\": \"typed_headers/content_type/video\",
                                \"package\": \"typed_headers\",
                                \"parameters\": []
                            }
                        }
                    ],
                    \"documentation\": null
                },
                {
                    \"name\": \"VideoWithCodecs\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": null,
                            \"params_type\": {
                                \"ref\": \"1.1.0\",
                                \"name\": \"Video\",
                                \"type\": \"named\",
                                \"module\": \"typed_headers/content_type/video\",
                                \"package\": \"typed_headers\",
                                \"parameters\": []
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"codecs\",
                            \"params_type\": {
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
                            }
                        }
                    ],
                    \"documentation\": null
                },
                {
                    \"name\": \"CustomContentType\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": \"media_type\",
                            \"params_type\": {
                                \"ref\": null,
                                \"name\": \"String\",
                                \"type\": \"named\",
                                \"module\": \"gleam\",
                                \"package\": \"\",
                                \"parameters\": []
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"sub_type\",
                            \"params_type\": {
                                \"ref\": null,
                                \"name\": \"String\",
                                \"type\": \"named\",
                                \"module\": \"gleam\",
                                \"package\": \"\",
                                \"parameters\": []
                            }
                        }
                    ],
                    \"documentation\": null
                },
                {
                    \"name\": \"CustomRawContentType\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": null,
                            \"params_type\": {
                                \"ref\": null,
                                \"name\": \"String\",
                                \"type\": \"named\",
                                \"module\": \"gleam\",
                                \"package\": \"\",
                                \"parameters\": []
                            }
                        }
                    ],
                    \"documentation\": null
                }
            ],
            \"documentation\": null
        },
        \"module_name\": \"typed_headers/content_type\",
        \"package_name\": \"typed_headers\",
        \"version\": \"1.1.0\"
    },
    {
        \"name\": \"Application\",
        \"documentation\": \"\",
        \"nature\": \"type_definition\",
        \"metadata\": {
            \"deprecation\": null
        },
        \"json_signature\": {
            \"name\": \"Application\",
            \"type\": \"type-definition\",
            \"parameters\": 0,
            \"deprecation\": null,
            \"constructors\": [
                {
                    \"name\": \"JSON\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [],
                    \"documentation\": null
                },
                {
                    \"name\": \"XML\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [],
                    \"documentation\": null
                },
                {
                    \"name\": \"OGG\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [],
                    \"documentation\": null
                },
                {
                    \"name\": \"Custom\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": null,
                            \"params_type\": {
                                \"ref\": null,
                                \"name\": \"String\",
                                \"type\": \"named\",
                                \"module\": \"gleam\",
                                \"package\": \"\",
                                \"parameters\": []
                            }
                        }
                    ],
                    \"documentation\": null
                }
            ],
            \"documentation\": null
        },
        \"module_name\": \"typed_headers/content_type/application\",
        \"package_name\": \"typed_headers\",
        \"version\": \"1.1.2\"
    },
    {
        \"name\": \"Application\",
        \"documentation\": \"\",
        \"nature\": \"type_definition\",
        \"metadata\": {
            \"deprecation\": null
        },
        \"json_signature\": {
            \"name\": \"Application\",
            \"type\": \"type-definition\",
            \"parameters\": 0,
            \"deprecation\": null,
            \"constructors\": [
                {
                    \"name\": \"JSON\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [],
                    \"documentation\": null
                },
                {
                    \"name\": \"XML\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [],
                    \"documentation\": null
                },
                {
                    \"name\": \"OGG\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [],
                    \"documentation\": null
                },
                {
                    \"name\": \"OGGWithCodecs\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": \"codecs\",
                            \"params_type\": {
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
                            }
                        }
                    ],
                    \"documentation\": null
                },
                {
                    \"name\": \"Custom\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": null,
                            \"params_type\": {
                                \"ref\": null,
                                \"name\": \"String\",
                                \"type\": \"named\",
                                \"module\": \"gleam\",
                                \"package\": \"\",
                                \"parameters\": []
                            }
                        }
                    ],
                    \"documentation\": null
                }
            ],
            \"documentation\": null
        },
        \"module_name\": \"typed_headers/content_type/application\",
        \"package_name\": \"typed_headers\",
        \"version\": \"1.1.0\"
    },
    {
        \"name\": \"application_stopped\",
        \"documentation\": \"\",
        \"nature\": \"function\",
        \"metadata\": {
            \"deprecation\": null,
            \"implementations\": {
                \"gleam\": false,
                \"uses_erlang_externals\": true,
                \"uses_javascript_externals\": false
            }
        },
        \"json_signature\": {
            \"name\": \"application_stopped\",
            \"type\": \"function\",
            \"return\": {
                \"ref\": \"0.8.0\",
                \"name\": \"ApplicationStop\",
                \"type\": \"named\",
                \"module\": \"gleam/otp/supervisor\",
                \"package\": \"gleam_otp\",
                \"parameters\": []
            },
            \"parameters\": [],
            \"deprecation\": null,
            \"documentation\": null,
            \"implementations\": {
                \"gleam\": false,
                \"uses_erlang_externals\": true,
                \"uses_javascript_externals\": false
            }
        },
        \"module_name\": \"gleam/otp/supervisor\",
        \"package_name\": \"gleam_otp\",
        \"version\": \"0.8.0\"
    },
    {
        \"name\": \"ensure_all_started\",
        \"documentation\": \"Starts an OTP application's process tree in the background, as well as\\n the trees of any applications that the given application depends upon. An\\n OTP application typically maps onto a Gleam or Hex package.\\n\\n Returns a list of the applications that were started. Calling this function\\n for application that have already been started is a no-op so you do not need\\n to check the application state beforehand.\\n\\n In Gleam we prefer to not use these implicit background process trees, but\\n you will likely still need to start the trees of OTP applications written in\\n other BEAM languages such as Erlang or Elixir, including those included by\\n default with Erlang/OTP.\\n\\n For more information see the OTP documentation.\\n - <https://www.erlang.org/doc/man/application.html#ensure_all_started-1>\\n - <https://www.erlang.org/doc/man/application.html#start-1>\",
        \"nature\": \"function\",
        \"metadata\": {
            \"deprecation\": null,
            \"implementations\": {
                \"gleam\": false,
                \"uses_erlang_externals\": true,
                \"uses_javascript_externals\": false
            }
        },
        \"json_signature\": {
            \"name\": \"ensure_all_started\",
            \"type\": \"function\",
            \"return\": {
                \"ref\": null,
                \"name\": \"Result\",
                \"type\": \"named\",
                \"module\": \"gleam\",
                \"package\": \"\",
                \"parameters\": [
                    {
                        \"ref\": null,
                        \"name\": \"List\",
                        \"type\": \"named\",
                        \"module\": \"gleam\",
                        \"package\": \"\",
                        \"parameters\": [
                            {
                                \"ref\": \"0.24.0\",
                                \"name\": \"Atom\",
                                \"type\": \"named\",
                                \"module\": \"gleam/erlang/atom\",
                                \"package\": \"gleam_erlang\",
                                \"parameters\": []
                            }
                        ]
                    },
                    {
                        \"ref\": \"0.24.0\",
                        \"name\": \"EnsureAllStartedError\",
                        \"type\": \"named\",
                        \"module\": \"gleam/erlang\",
                        \"package\": \"gleam_erlang\",
                        \"parameters\": []
                    }
                ]
            },
            \"parameters\": [
                {
                    \"type\": \"parameter\",
                    \"label\": \"application\",
                    \"params_type\": {
                        \"ref\": \"0.24.0\",
                        \"name\": \"Atom\",
                        \"type\": \"named\",
                        \"module\": \"gleam/erlang/atom\",
                        \"package\": \"gleam_erlang\",
                        \"parameters\": []
                    }
                }
            ],
            \"deprecation\": \" Starts an OTP application's process tree in the background, as well as\\n the trees of any applications that the given application depends upon. An\\n OTP application typically maps onto a Gleam or Hex package.\\n\\n Returns a list of the applications that were started. Calling this function\\n for application that have already been started is a no-op so you do not need\\n to check the application state beforehand.\\n\\n In Gleam we prefer to not use these implicit background process trees, but\\n you will likely still need to start the trees of OTP applications written in\\n other BEAM languages such as Erlang or Elixir, including those included by\\n default with Erlang/OTP.\\n\\n For more information see the OTP documentation.\\n - <https://www.erlang.org/doc/man/application.html#ensure_all_started-1>\\n - <https://www.erlang.org/doc/man/application.html#start-1>\\n\",
            \"documentation\": \" Starts an OTP application's process tree in the background, as well as\\n the trees of any applications that the given application depends upon. An\\n OTP application typically maps onto a Gleam or Hex package.\\n\\n Returns a list of the applications that were started. Calling this function\\n for application that have already been started is a no-op so you do not need\\n to check the application state beforehand.\\n\\n In Gleam we prefer to not use these implicit background process trees, but\\n you will likely still need to start the trees of OTP applications written in\\n other BEAM languages such as Erlang or Elixir, including those included by\\n default with Erlang/OTP.\\n\\n For more information see the OTP documentation.\\n - <https://www.erlang.org/doc/man/application.html#ensure_all_started-1>\\n - <https://www.erlang.org/doc/man/application.html#start-1>\\n\",
            \"implementations\": {
                \"gleam\": false,
                \"uses_erlang_externals\": true,
                \"uses_javascript_externals\": false
            }
        },
        \"module_name\": \"gleam/erlang\",
        \"package_name\": \"gleam_erlang\",
        \"version\": \"0.24.0\"
    },
    {
        \"name\": \"application\",
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
            \"name\": \"application\",
            \"type\": \"function\",
            \"return\": {
                \"ref\": \"3.1.4\",
                \"name\": \"App\",
                \"type\": \"named\",
                \"module\": \"lustre\",
                \"package\": \"lustre\",
                \"parameters\": [
                    {
                        \"id\": 0,
                        \"type\": \"variable\"
                    },
                    {
                        \"id\": 1,
                        \"type\": \"variable\"
                    },
                    {
                        \"id\": 2,
                        \"type\": \"variable\"
                    }
                ]
            },
            \"parameters\": [
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
                        \"type\": \"fn\",
                        \"params\": [
                            {
                                \"id\": 0,
                                \"type\": \"variable\"
                            }
                        ],
                        \"return\": {
                            \"type\": \"tuple\",
                            \"elements\": [
                                {
                                    \"id\": 1,
                                    \"type\": \"variable\"
                                },
                                {
                                    \"ref\": \"3.1.4\",
                                    \"name\": \"Effect\",
                                    \"type\": \"named\",
                                    \"module\": \"lustre/effect\",
                                    \"package\": \"lustre\",
                                    \"parameters\": [
                                        {
                                            \"id\": 2,
                                            \"type\": \"variable\"
                                        }
                                    ]
                                }
                            ]
                        }
                    }
                },
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
                        \"type\": \"fn\",
                        \"params\": [
                            {
                                \"id\": 1,
                                \"type\": \"variable\"
                            },
                            {
                                \"id\": 2,
                                \"type\": \"variable\"
                            }
                        ],
                        \"return\": {
                            \"type\": \"tuple\",
                            \"elements\": [
                                {
                                    \"id\": 1,
                                    \"type\": \"variable\"
                                },
                                {
                                    \"ref\": \"3.1.4\",
                                    \"name\": \"Effect\",
                                    \"type\": \"named\",
                                    \"module\": \"lustre/effect\",
                                    \"package\": \"lustre\",
                                    \"parameters\": [
                                        {
                                            \"id\": 2,
                                            \"type\": \"variable\"
                                        }
                                    ]
                                }
                            ]
                        }
                    }
                },
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
                        \"type\": \"fn\",
                        \"params\": [
                            {
                                \"id\": 1,
                                \"type\": \"variable\"
                            }
                        ],
                        \"return\": {
                            \"ref\": \"3.1.4\",
                            \"name\": \"Element\",
                            \"type\": \"named\",
                            \"module\": \"lustre/element\",
                            \"package\": \"lustre\",
                            \"parameters\": [
                                {
                                    \"id\": 2,
                                    \"type\": \"variable\"
                                }
                            ]
                        }
                    }
                }
            ],
            \"deprecation\": \"\",
            \"documentation\": \"\",
            \"implementations\": {
                \"gleam\": false,
                \"uses_erlang_externals\": true,
                \"uses_javascript_externals\": true
            }
        },
        \"module_name\": \"lustre\",
        \"package_name\": \"lustre\",
        \"version\": \"3.1.4\"
    },
    {
        \"name\": \"application_decoder\",
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
            \"name\": \"application_decoder\",
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
                            \"name\": \"Application\",
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
        \"name\": \"application\",
        \"documentation\": \"\",
        \"nature\": \"function\",
        \"metadata\": {
            \"deprecation\": null,
            \"implementations\": {
                \"gleam\": true,
                \"uses_erlang_externals\": true,
                \"uses_javascript_externals\": false
            }
        },
        \"json_signature\": {
            \"name\": \"application\",
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
                            \"ref\": \"0.0.1\",
                            \"name\": \"Application\",
                            \"type\": \"named\",
                            \"module\": \"models/api\",
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
            \"parameters\": [
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
                        \"type\": \"fn\",
                        \"params\": [
                            {
                                \"id\": 0,
                                \"type\": \"variable\"
                            },
                            {
                                \"id\": 1,
                                \"type\": \"variable\"
                            },
                            {
                                \"id\": 2,
                                \"type\": \"variable\"
                            },
                            {
                                \"id\": 3,
                                \"type\": \"variable\"
                            },
                            {
                                \"id\": 4,
                                \"type\": \"variable\"
                            },
                            {
                                \"id\": 5,
                                \"type\": \"variable\"
                            },
                            {
                                \"id\": 6,
                                \"type\": \"variable\"
                            },
                            {
                                \"id\": 7,
                                \"type\": \"variable\"
                            },
                            {
                                \"id\": 8,
                                \"type\": \"variable\"
                            },
                            {
                                \"id\": 9,
                                \"type\": \"variable\"
                            },
                            {
                                \"id\": 10,
                                \"type\": \"variable\"
                            },
                            {
                                \"id\": 11,
                                \"type\": \"variable\"
                            },
                            {
                                \"id\": 12,
                                \"type\": \"variable\"
                            },
                            {
                                \"id\": 13,
                                \"type\": \"variable\"
                            },
                            {
                                \"id\": 14,
                                \"type\": \"variable\"
                            },
                            {
                                \"id\": 15,
                                \"type\": \"variable\"
                            },
                            {
                                \"id\": 16,
                                \"type\": \"variable\"
                            },
                            {
                                \"id\": 17,
                                \"type\": \"variable\"
                            },
                            {
                                \"id\": 18,
                                \"type\": \"variable\"
                            },
                            {
                                \"id\": 19,
                                \"type\": \"variable\"
                            },
                            {
                                \"id\": 20,
                                \"type\": \"variable\"
                            },
                            {
                                \"id\": 21,
                                \"type\": \"variable\"
                            },
                            {
                                \"id\": 22,
                                \"type\": \"variable\"
                            },
                            {
                                \"id\": 23,
                                \"type\": \"variable\"
                            },
                            {
                                \"id\": 24,
                                \"type\": \"variable\"
                            },
                            {
                                \"id\": 25,
                                \"type\": \"variable\"
                            }
                        ],
                        \"return\": {
                            \"ref\": \"0.0.1\",
                            \"name\": \"Application\",
                            \"type\": \"named\",
                            \"module\": \"models/api\",
                            \"package\": \"glyph\",
                            \"parameters\": []
                        }
                    }
                },
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
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
                                    \"id\": 0,
                                    \"type\": \"variable\"
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
                    }
                },
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
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
                                    \"id\": 1,
                                    \"type\": \"variable\"
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
                    }
                },
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
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
                                    \"id\": 2,
                                    \"type\": \"variable\"
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
                    }
                },
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
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
                                    \"id\": 3,
                                    \"type\": \"variable\"
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
                    }
                },
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
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
                                    \"id\": 4,
                                    \"type\": \"variable\"
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
                    }
                },
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
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
                                    \"id\": 5,
                                    \"type\": \"variable\"
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
                    }
                },
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
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
                                    \"id\": 6,
                                    \"type\": \"variable\"
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
                    }
                },
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
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
                                    \"id\": 7,
                                    \"type\": \"variable\"
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
                    }
                },
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
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
                                    \"id\": 8,
                                    \"type\": \"variable\"
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
                    }
                },
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
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
                                    \"id\": 9,
                                    \"type\": \"variable\"
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
                    }
                },
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
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
                                    \"id\": 10,
                                    \"type\": \"variable\"
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
                    }
                },
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
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
                                    \"id\": 11,
                                    \"type\": \"variable\"
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
                    }
                },
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
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
                                    \"id\": 12,
                                    \"type\": \"variable\"
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
                    }
                },
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
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
                                    \"id\": 13,
                                    \"type\": \"variable\"
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
                    }
                },
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
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
                                    \"id\": 14,
                                    \"type\": \"variable\"
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
                    }
                },
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
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
                                    \"id\": 15,
                                    \"type\": \"variable\"
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
                    }
                },
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
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
                                    \"id\": 16,
                                    \"type\": \"variable\"
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
                    }
                },
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
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
                                    \"id\": 17,
                                    \"type\": \"variable\"
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
                    }
                },
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
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
                                    \"id\": 18,
                                    \"type\": \"variable\"
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
                    }
                },
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
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
                                    \"id\": 19,
                                    \"type\": \"variable\"
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
                    }
                },
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
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
                                    \"id\": 20,
                                    \"type\": \"variable\"
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
                    }
                },
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
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
                                    \"id\": 21,
                                    \"type\": \"variable\"
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
                    }
                },
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
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
                                    \"id\": 22,
                                    \"type\": \"variable\"
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
                    }
                },
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
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
                                    \"id\": 23,
                                    \"type\": \"variable\"
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
                    }
                },
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
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
                                    \"id\": 24,
                                    \"type\": \"variable\"
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
                    }
                },
                {
                    \"type\": \"parameter\",
                    \"label\": null,
                    \"params_type\": {
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
                                    \"id\": 25,
                                    \"type\": \"variable\"
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
                    }
                }
            ],
            \"deprecation\": null,
            \"documentation\": null,
            \"implementations\": {
                \"gleam\": true,
                \"uses_erlang_externals\": true,
                \"uses_javascript_externals\": false
            }
        },
        \"module_name\": \"models/decoders\",
        \"package_name\": \"glyph\",
        \"version\": \"0.0.1\"
    },
    {
        \"name\": \"Argon2Type\",
        \"documentation\": \"\",
        \"nature\": \"type_definition\",
        \"metadata\": {
            \"deprecation\": null
        },
        \"json_signature\": {
            \"name\": \"Argon2Type\",
            \"type\": \"type-definition\",
            \"parameters\": 0,
            \"deprecation\": null,
            \"constructors\": [
                {
                    \"name\": \"Argon2d\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [],
                    \"documentation\": \" Suitable for applications with no threats from side-channel timing attacks (eg. cryptocurrencies)\"
                },
                {
                    \"name\": \"Argon2i\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [],
                    \"documentation\": \" Suitable for password hashing and password-based key derivation.\"
                },
                {
                    \"name\": \"Argon2id\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [],
                    \"documentation\": \" A hybrid of Argon2d and Argon2i. The default type.\"
                }
            ],
            \"documentation\": null
        },
        \"module_name\": \"antigone\",
        \"package_name\": \"antigone\",
        \"version\": \"1.0.0\"
    },
    {
        \"name\": \"Argon2Type\",
        \"documentation\": \"\",
        \"nature\": \"type_definition\",
        \"metadata\": {
            \"deprecation\": null
        },
        \"json_signature\": {
            \"name\": \"Argon2Type\",
            \"type\": \"type-definition\",
            \"parameters\": 0,
            \"deprecation\": null,
            \"constructors\": [
                {
                    \"name\": \"Argon2d\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [],
                    \"documentation\": \" Suitable for applications with no threats from side-channel timing attacks (eg. cryptocurrencies)\"
                },
                {
                    \"name\": \"Argon2i\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [],
                    \"documentation\": \" Suitable for password hashing and password-based key derivation.\"
                },
                {
                    \"name\": \"Argon2id\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [],
                    \"documentation\": \" A hybrid of Argon2d and Argon2i. The default type.\"
                }
            ],
            \"documentation\": null
        },
        \"module_name\": \"antigone\",
        \"package_name\": \"antigone\",
        \"version\": \"1.1.0\"
    },
    {
        \"name\": \"ContentType\",
        \"documentation\": \"\",
        \"nature\": \"type_definition\",
        \"metadata\": {
            \"deprecation\": null
        },
        \"json_signature\": {
            \"name\": \"ContentType\",
            \"type\": \"type-definition\",
            \"parameters\": 0,
            \"deprecation\": null,
            \"constructors\": [
                {
                    \"name\": \"Application\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": null,
                            \"params_type\": {
                                \"ref\": \"1.1.2\",
                                \"name\": \"Application\",
                                \"type\": \"named\",
                                \"module\": \"typed_headers/content_type/application\",
                                \"package\": \"typed_headers\",
                                \"parameters\": []
                            }
                        }
                    ],
                    \"documentation\": null
                },
                {
                    \"name\": \"ApplicationWithCodecs\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": null,
                            \"params_type\": {
                                \"ref\": \"1.1.2\",
                                \"name\": \"Application\",
                                \"type\": \"named\",
                                \"module\": \"typed_headers/content_type/application\",
                                \"package\": \"typed_headers\",
                                \"parameters\": []
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"codecs\",
                            \"params_type\": {
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
                            }
                        }
                    ],
                    \"documentation\": null
                },
                {
                    \"name\": \"Audio\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": null,
                            \"params_type\": {
                                \"ref\": \"1.1.2\",
                                \"name\": \"Audio\",
                                \"type\": \"named\",
                                \"module\": \"typed_headers/content_type/audio\",
                                \"package\": \"typed_headers\",
                                \"parameters\": []
                            }
                        }
                    ],
                    \"documentation\": null
                },
                {
                    \"name\": \"AudioWithCodecs\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": null,
                            \"params_type\": {
                                \"ref\": \"1.1.2\",
                                \"name\": \"Audio\",
                                \"type\": \"named\",
                                \"module\": \"typed_headers/content_type/audio\",
                                \"package\": \"typed_headers\",
                                \"parameters\": []
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"codecs\",
                            \"params_type\": {
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
                            }
                        }
                    ],
                    \"documentation\": null
                },
                {
                    \"name\": \"Font\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": null,
                            \"params_type\": {
                                \"ref\": \"1.1.2\",
                                \"name\": \"Font\",
                                \"type\": \"named\",
                                \"module\": \"typed_headers/content_type/font\",
                                \"package\": \"typed_headers\",
                                \"parameters\": []
                            }
                        }
                    ],
                    \"documentation\": null
                },
                {
                    \"name\": \"Example\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": null,
                            \"params_type\": {
                                \"ref\": \"1.1.2\",
                                \"name\": \"Example\",
                                \"type\": \"named\",
                                \"module\": \"typed_headers/content_type/example\",
                                \"package\": \"typed_headers\",
                                \"parameters\": []
                            }
                        }
                    ],
                    \"documentation\": null
                },
                {
                    \"name\": \"Image\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": null,
                            \"params_type\": {
                                \"ref\": \"1.1.2\",
                                \"name\": \"Image\",
                                \"type\": \"named\",
                                \"module\": \"typed_headers/content_type/image\",
                                \"package\": \"typed_headers\",
                                \"parameters\": []
                            }
                        }
                    ],
                    \"documentation\": null
                },
                {
                    \"name\": \"Message\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": null,
                            \"params_type\": {
                                \"ref\": \"1.1.2\",
                                \"name\": \"Message\",
                                \"type\": \"named\",
                                \"module\": \"typed_headers/content_type/message\",
                                \"package\": \"typed_headers\",
                                \"parameters\": []
                            }
                        }
                    ],
                    \"documentation\": null
                },
                {
                    \"name\": \"Model\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": null,
                            \"params_type\": {
                                \"ref\": \"1.1.2\",
                                \"name\": \"Model\",
                                \"type\": \"named\",
                                \"module\": \"typed_headers/content_type/model\",
                                \"package\": \"typed_headers\",
                                \"parameters\": []
                            }
                        }
                    ],
                    \"documentation\": null
                },
                {
                    \"name\": \"Multipart\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": null,
                            \"params_type\": {
                                \"ref\": \"1.1.2\",
                                \"name\": \"Multipart\",
                                \"type\": \"named\",
                                \"module\": \"typed_headers/content_type/multipart\",
                                \"package\": \"typed_headers\",
                                \"parameters\": []
                            }
                        }
                    ],
                    \"documentation\": null
                },
                {
                    \"name\": \"Text\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": null,
                            \"params_type\": {
                                \"ref\": \"1.1.2\",
                                \"name\": \"Text\",
                                \"type\": \"named\",
                                \"module\": \"typed_headers/content_type/text\",
                                \"package\": \"typed_headers\",
                                \"parameters\": []
                            }
                        }
                    ],
                    \"documentation\": null
                },
                {
                    \"name\": \"Video\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": null,
                            \"params_type\": {
                                \"ref\": \"1.1.2\",
                                \"name\": \"Video\",
                                \"type\": \"named\",
                                \"module\": \"typed_headers/content_type/video\",
                                \"package\": \"typed_headers\",
                                \"parameters\": []
                            }
                        }
                    ],
                    \"documentation\": null
                },
                {
                    \"name\": \"VideoWithCodecs\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": null,
                            \"params_type\": {
                                \"ref\": \"1.1.2\",
                                \"name\": \"Video\",
                                \"type\": \"named\",
                                \"module\": \"typed_headers/content_type/video\",
                                \"package\": \"typed_headers\",
                                \"parameters\": []
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"codecs\",
                            \"params_type\": {
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
                            }
                        }
                    ],
                    \"documentation\": null
                },
                {
                    \"name\": \"Custom\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": \"media_type\",
                            \"params_type\": {
                                \"ref\": null,
                                \"name\": \"String\",
                                \"type\": \"named\",
                                \"module\": \"gleam\",
                                \"package\": \"\",
                                \"parameters\": []
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"sub_type\",
                            \"params_type\": {
                                \"ref\": null,
                                \"name\": \"String\",
                                \"type\": \"named\",
                                \"module\": \"gleam\",
                                \"package\": \"\",
                                \"parameters\": []
                            }
                        }
                    ],
                    \"documentation\": null
                },
                {
                    \"name\": \"Raw\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": null,
                            \"params_type\": {
                                \"ref\": null,
                                \"name\": \"String\",
                                \"type\": \"named\",
                                \"module\": \"gleam\",
                                \"package\": \"\",
                                \"parameters\": []
                            }
                        }
                    ],
                    \"documentation\": null
                }
            ],
            \"documentation\": null
        },
        \"module_name\": \"typed_headers/content_type\",
        \"package_name\": \"typed_headers\",
        \"version\": \"1.1.2\"
    },
    {
        \"name\": \"ReadyEvent\",
        \"documentation\": \"Structure of a Ready event: https://discord.com/developers/docs/topics/gateway-events#ready-ready-event-fields\",
        \"nature\": \"type_definition\",
        \"metadata\": {
            \"deprecation\": null
        },
        \"json_signature\": {
            \"name\": \"ReadyEvent\",
            \"type\": \"type-definition\",
            \"parameters\": 0,
            \"deprecation\": \" Structure of a Ready event: https://discord.com/developers/docs/topics/gateway-events#ready-ready-event-fields\",
            \"constructors\": [
                {
                    \"name\": \"ReadyEvent\",
                    \"type\": \"type-constructor\",
                    \"parameters\": [
                        {
                            \"type\": \"parameter\",
                            \"label\": \"v\",
                            \"params_type\": {
                                \"ref\": null,
                                \"name\": \"Int\",
                                \"type\": \"named\",
                                \"module\": \"gleam\",
                                \"package\": \"\",
                                \"parameters\": []
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"user\",
                            \"params_type\": {
                                \"ref\": \"0.1.0\",
                                \"name\": \"User\",
                                \"type\": \"named\",
                                \"module\": \"glyph/models/discord\",
                                \"package\": \"glyph\",
                                \"parameters\": []
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"guilds\",
                            \"params_type\": {
                                \"ref\": \"0.37.0\",
                                \"name\": \"Dynamic\",
                                \"type\": \"named\",
                                \"module\": \"gleam/dynamic\",
                                \"package\": \"gleam_stdlib\",
                                \"parameters\": []
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"session_id\",
                            \"params_type\": {
                                \"ref\": null,
                                \"name\": \"String\",
                                \"type\": \"named\",
                                \"module\": \"gleam\",
                                \"package\": \"\",
                                \"parameters\": []
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"resume_gateway_url\",
                            \"params_type\": {
                                \"ref\": null,
                                \"name\": \"String\",
                                \"type\": \"named\",
                                \"module\": \"gleam\",
                                \"package\": \"\",
                                \"parameters\": []
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"shard\",
                            \"params_type\": {
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
                                                \"ref\": null,
                                                \"name\": \"Int\",
                                                \"type\": \"named\",
                                                \"module\": \"gleam\",
                                                \"package\": \"\",
                                                \"parameters\": []
                                            }
                                        ]
                                    }
                                ]
                            }
                        },
                        {
                            \"type\": \"parameter\",
                            \"label\": \"application\",
                            \"params_type\": {
                                \"ref\": \"0.1.0\",
                                \"name\": \"ReadyApplication\",
                                \"type\": \"named\",
                                \"module\": \"glyph/models/discord\",
                                \"package\": \"glyph\",
                                \"parameters\": []
                            }
                        }
                    ],
                    \"documentation\": null
                }
            ],
            \"documentation\": \" Structure of a Ready event: https://discord.com/developers/docs/topics/gateway-events#ready-ready-event-fields\"
        },
        \"module_name\": \"glyph/models/discord\",
        \"package_name\": \"glyph\",
        \"version\": \"0.1.0\"
    }
]"
  |> json.decode(search_result.decode_search_results)
}
