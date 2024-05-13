import data/search_result
import gleam/json

pub fn mock() {
  "{
    \"exact-matches\": [
        {
            \"name\": \"append\",
            \"documentation\": \"Joins a document into the end of another.\\n\\n ## Examples\\n\\n ```gleam\\n from_string(\\\"pretty\\\")\\n |> append(from_string(\\\" printer\\\"))\\n |> to_string(80)\\n // -> \\\"pretty printer\\\"\\n ```\",
            \"kind\": \"function\",
            \"metadata\": {
                \"deprecation\": null,
                \"implementations\": {
                    \"gleam\": true,
                    \"uses_erlang_externals\": true,
                    \"uses_javascript_externals\": false
                }
            },
            \"json_signature\": {
                \"kind\": \"function\",
                \"name\": \"append\",
                \"return\": {
                    \"ref\": \"2.0.1\",
                    \"kind\": \"named\",
                    \"name\": \"Document\",
                    \"module\": \"glam/doc\",
                    \"package\": \"glam\",
                    \"parameters\": []
                },
                \"parameters\": [
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": \"2.0.1\",
                            \"kind\": \"named\",
                            \"name\": \"Document\",
                            \"module\": \"glam/doc\",
                            \"package\": \"glam\",
                            \"parameters\": []
                        },
                        \"label\": \"to\"
                    },
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": \"2.0.1\",
                            \"kind\": \"named\",
                            \"name\": \"Document\",
                            \"module\": \"glam/doc\",
                            \"package\": \"glam\",
                            \"parameters\": []
                        },
                        \"label\": \"doc\"
                    }
                ],
                \"deprecation\": \" Joins a document into the end of another.\\n\\n ## Examples\\n\\n ```gleam\\n from_string(\\\"pretty\\\")\\n |> append(from_string(\\\" printer\\\"))\\n |> to_string(80)\\n // -> \\\"pretty printer\\\"\\n ```\\n\",
                \"documentation\": \" Joins a document into the end of another.\\n\\n ## Examples\\n\\n ```gleam\\n from_string(\\\"pretty\\\")\\n |> append(from_string(\\\" printer\\\"))\\n |> to_string(80)\\n // -> \\\"pretty printer\\\"\\n ```\\n\",
                \"implementations\": {
                    \"gleam\": true,
                    \"uses_erlang_externals\": true,
                    \"uses_javascript_externals\": false
                }
            },
            \"module_name\": \"glam/doc\",
            \"package_name\": \"glam\",
            \"version\": \"2.0.1\"
        },
        {
            \"name\": \"append\",
            \"documentation\": \"Creates a new bit array by joining two bit arrays.\\n\\n ## Examples\\n\\n ```gleam\\n append(to: from_string(\\\"butter\\\"), suffix: from_string(\\\"fly\\\"))\\n // -> from_string(\\\"butterfly\\\")\\n ```\",
            \"kind\": \"function\",
            \"metadata\": {
                \"deprecation\": null,
                \"implementations\": {
                    \"gleam\": false,
                    \"uses_erlang_externals\": true,
                    \"uses_javascript_externals\": true
                }
            },
            \"json_signature\": {
                \"kind\": \"function\",
                \"name\": \"append\",
                \"return\": {
                    \"ref\": null,
                    \"kind\": \"named\",
                    \"name\": \"BitArray\",
                    \"module\": \"gleam\",
                    \"package\": \"\",
                    \"parameters\": []
                },
                \"parameters\": [
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": null,
                            \"kind\": \"named\",
                            \"name\": \"BitArray\",
                            \"module\": \"gleam\",
                            \"package\": \"\",
                            \"parameters\": []
                        },
                        \"label\": \"to\"
                    },
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": null,
                            \"kind\": \"named\",
                            \"name\": \"BitArray\",
                            \"module\": \"gleam\",
                            \"package\": \"\",
                            \"parameters\": []
                        },
                        \"label\": \"suffix\"
                    }
                ],
                \"deprecation\": \" Creates a new bit array by joining two bit arrays.\\n\\n ## Examples\\n\\n ```gleam\\n append(to: from_string(\\\"butter\\\"), suffix: from_string(\\\"fly\\\"))\\n // -> from_string(\\\"butterfly\\\")\\n ```\\n\",
                \"documentation\": \" Creates a new bit array by joining two bit arrays.\\n\\n ## Examples\\n\\n ```gleam\\n append(to: from_string(\\\"butter\\\"), suffix: from_string(\\\"fly\\\"))\\n // -> from_string(\\\"butterfly\\\")\\n ```\\n\",
                \"implementations\": {
                    \"gleam\": false,
                    \"uses_erlang_externals\": true,
                    \"uses_javascript_externals\": true
                }
            },
            \"module_name\": \"gleam/bit_array\",
            \"package_name\": \"gleam_stdlib\",
            \"version\": \"0.37.0\"
        },
        {
            \"name\": \"append\",
            \"documentation\": \"\",
            \"kind\": \"function\",
            \"metadata\": {
                \"deprecation\": \"Please use the `gleam/bytes_builder` module instead.\",
                \"implementations\": {
                    \"gleam\": true,
                    \"uses_erlang_externals\": true,
                    \"uses_javascript_externals\": false
                }
            },
            \"json_signature\": {
                \"kind\": \"function\",
                \"name\": \"append\",
                \"return\": {
                    \"ref\": \"0.34.0\",
                    \"kind\": \"named\",
                    \"name\": \"BytesBuilder\",
                    \"module\": \"gleam/bytes_builder\",
                    \"package\": \"gleam_stdlib\",
                    \"parameters\": []
                },
                \"parameters\": [
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": \"0.34.0\",
                            \"kind\": \"named\",
                            \"name\": \"BytesBuilder\",
                            \"module\": \"gleam/bytes_builder\",
                            \"package\": \"gleam_stdlib\",
                            \"parameters\": []
                        },
                        \"label\": null
                    },
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": null,
                            \"kind\": \"named\",
                            \"name\": \"BitArray\",
                            \"module\": \"gleam\",
                            \"package\": \"\",
                            \"parameters\": []
                        },
                        \"label\": null
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
            \"module_name\": \"gleam/bit_builder\",
            \"package_name\": \"gleam_stdlib\",
            \"version\": \"0.34.0\"
        },
        {
            \"name\": \"append\",
            \"documentation\": \"\",
            \"kind\": \"function\",
            \"metadata\": {
                \"deprecation\": \"Please use the `gleam/bit_array` module instead.\",
                \"implementations\": {
                    \"gleam\": false,
                    \"uses_erlang_externals\": true,
                    \"uses_javascript_externals\": true
                }
            },
            \"json_signature\": {
                \"kind\": \"function\",
                \"name\": \"append\",
                \"return\": {
                    \"ref\": null,
                    \"kind\": \"named\",
                    \"name\": \"BitArray\",
                    \"module\": \"gleam\",
                    \"package\": \"\",
                    \"parameters\": []
                },
                \"parameters\": [
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": null,
                            \"kind\": \"named\",
                            \"name\": \"BitArray\",
                            \"module\": \"gleam\",
                            \"package\": \"\",
                            \"parameters\": []
                        },
                        \"label\": \"to\"
                    },
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": null,
                            \"kind\": \"named\",
                            \"name\": \"BitArray\",
                            \"module\": \"gleam\",
                            \"package\": \"\",
                            \"parameters\": []
                        },
                        \"label\": \"suffix\"
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
            \"module_name\": \"gleam/bit_string\",
            \"package_name\": \"gleam_stdlib\",
            \"version\": \"0.34.0\"
        },
        {
            \"name\": \"append\",
            \"documentation\": \"Appends a bit array to the end of a builder.\\n\\n Runs in constant time.\",
            \"kind\": \"function\",
            \"metadata\": {
                \"deprecation\": null,
                \"implementations\": {
                    \"gleam\": true,
                    \"uses_erlang_externals\": true,
                    \"uses_javascript_externals\": false
                }
            },
            \"json_signature\": {
                \"kind\": \"function\",
                \"name\": \"append\",
                \"return\": {
                    \"ref\": \"0.37.0\",
                    \"kind\": \"named\",
                    \"name\": \"BytesBuilder\",
                    \"module\": \"gleam/bytes_builder\",
                    \"package\": \"gleam_stdlib\",
                    \"parameters\": []
                },
                \"parameters\": [
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": \"0.37.0\",
                            \"kind\": \"named\",
                            \"name\": \"BytesBuilder\",
                            \"module\": \"gleam/bytes_builder\",
                            \"package\": \"gleam_stdlib\",
                            \"parameters\": []
                        },
                        \"label\": \"to\"
                    },
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": null,
                            \"kind\": \"named\",
                            \"name\": \"BitArray\",
                            \"module\": \"gleam\",
                            \"package\": \"\",
                            \"parameters\": []
                        },
                        \"label\": \"suffix\"
                    }
                ],
                \"deprecation\": \" Appends a bit array to the end of a builder.\\n\\n Runs in constant time.\\n\",
                \"documentation\": \" Appends a bit array to the end of a builder.\\n\\n Runs in constant time.\\n\",
                \"implementations\": {
                    \"gleam\": true,
                    \"uses_erlang_externals\": true,
                    \"uses_javascript_externals\": false
                }
            },
            \"module_name\": \"gleam/bytes_builder\",
            \"package_name\": \"gleam_stdlib\",
            \"version\": \"0.37.0\"
        },
        {
            \"name\": \"append\",
            \"documentation\": \"\",
            \"kind\": \"function\",
            \"metadata\": {
                \"deprecation\": null,
                \"implementations\": {
                    \"gleam\": true,
                    \"uses_erlang_externals\": false,
                    \"uses_javascript_externals\": false
                }
            },
            \"json_signature\": {
                \"kind\": \"function\",
                \"name\": \"append\",
                \"return\": {
                    \"ref\": \"0.1.0\",
                    \"kind\": \"named\",
                    \"name\": \"Path\",
                    \"module\": \"gleam_community/path\",
                    \"package\": \"gleam_community_path\",
                    \"parameters\": []
                },
                \"parameters\": [
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": \"0.1.0\",
                            \"kind\": \"named\",
                            \"name\": \"Path\",
                            \"module\": \"gleam_community/path\",
                            \"package\": \"gleam_community_path\",
                            \"parameters\": []
                        },
                        \"label\": null
                    },
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": \"0.1.0\",
                            \"kind\": \"named\",
                            \"name\": \"Path\",
                            \"module\": \"gleam_community/path\",
                            \"package\": \"gleam_community_path\",
                            \"parameters\": []
                        },
                        \"label\": null
                    }
                ],
                \"deprecation\": null,
                \"documentation\": null,
                \"implementations\": {
                    \"gleam\": true,
                    \"uses_erlang_externals\": false,
                    \"uses_javascript_externals\": false
                }
            },
            \"module_name\": \"gleam_community/path\",
            \"package_name\": \"gleam_community_path\",
            \"version\": \"0.1.0\"
        },
        {
            \"name\": \"append\",
            \"documentation\": \"\",
            \"kind\": \"function\",
            \"metadata\": {
                \"deprecation\": null,
                \"implementations\": {
                    \"gleam\": false,
                    \"uses_erlang_externals\": true,
                    \"uses_javascript_externals\": true
                }
            },
            \"json_signature\": {
                \"kind\": \"function\",
                \"name\": \"append\",
                \"return\": {
                    \"ref\": null,
                    \"kind\": \"named\",
                    \"name\": \"String\",
                    \"module\": \"gleam\",
                    \"package\": \"\",
                    \"parameters\": []
                },
                \"parameters\": [
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": null,
                            \"kind\": \"named\",
                            \"name\": \"String\",
                            \"module\": \"gleam\",
                            \"package\": \"\",
                            \"parameters\": []
                        },
                        \"label\": null
                    },
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": null,
                            \"kind\": \"named\",
                            \"name\": \"String\",
                            \"module\": \"gleam\",
                            \"package\": \"\",
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
            \"module_name\": \"gleam_community/path_string\",
            \"package_name\": \"gleam_community_path\",
            \"version\": \"0.1.0\"
        },
        {
            \"name\": \"append\",
            \"documentation\": \"Appends two iterators, producing a new iterator.\\n\\n This function does not evaluate the elements of the iterators, the\\n computation is performed when the resulting iterator is later run.\\n\\n ## Examples\\n\\n ```gleam\\n from_list([1, 2])\\n |> append(from_list([3, 4]))\\n |> to_list\\n // -> [1, 2, 3, 4]\\n ```\",
            \"kind\": \"function\",
            \"metadata\": {
                \"deprecation\": null,
                \"implementations\": {
                    \"gleam\": true,
                    \"uses_erlang_externals\": false,
                    \"uses_javascript_externals\": false
                }
            },
            \"json_signature\": {
                \"kind\": \"function\",
                \"name\": \"append\",
                \"return\": {
                    \"ref\": \"0.37.0\",
                    \"kind\": \"named\",
                    \"name\": \"Iterator\",
                    \"module\": \"gleam/iterator\",
                    \"package\": \"gleam_stdlib\",
                    \"parameters\": [
                        {
                            \"id\": 0,
                            \"kind\": \"variable\"
                        }
                    ]
                },
                \"parameters\": [
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": \"0.37.0\",
                            \"kind\": \"named\",
                            \"name\": \"Iterator\",
                            \"module\": \"gleam/iterator\",
                            \"package\": \"gleam_stdlib\",
                            \"parameters\": [
                                {
                                    \"id\": 0,
                                    \"kind\": \"variable\"
                                }
                            ]
                        },
                        \"label\": \"to\"
                    },
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": \"0.37.0\",
                            \"kind\": \"named\",
                            \"name\": \"Iterator\",
                            \"module\": \"gleam/iterator\",
                            \"package\": \"gleam_stdlib\",
                            \"parameters\": [
                                {
                                    \"id\": 0,
                                    \"kind\": \"variable\"
                                }
                            ]
                        },
                        \"label\": \"suffix\"
                    }
                ],
                \"deprecation\": \" Appends two iterators, producing a new iterator.\\n\\n This function does not evaluate the elements of the iterators, the\\n computation is performed when the resulting iterator is later run.\\n\\n ## Examples\\n\\n ```gleam\\n from_list([1, 2])\\n |> append(from_list([3, 4]))\\n |> to_list\\n // -> [1, 2, 3, 4]\\n ```\\n\",
                \"documentation\": \" Appends two iterators, producing a new iterator.\\n\\n This function does not evaluate the elements of the iterators, the\\n computation is performed when the resulting iterator is later run.\\n\\n ## Examples\\n\\n ```gleam\\n from_list([1, 2])\\n |> append(from_list([3, 4]))\\n |> to_list\\n // -> [1, 2, 3, 4]\\n ```\\n\",
                \"implementations\": {
                    \"gleam\": true,
                    \"uses_erlang_externals\": false,
                    \"uses_javascript_externals\": false
                }
            },
            \"module_name\": \"gleam/iterator\",
            \"package_name\": \"gleam_stdlib\",
            \"version\": \"0.37.0\"
        },
        {
            \"name\": \"append\",
            \"documentation\": \"Joins one list onto the end of another.\\n\\n This function runs in linear time, and it traverses and copies the first\\n list.\\n\\n ## Examples\\n\\n ```gleam\\n append([1, 2], [3])\\n // -> [1, 2, 3]\\n ```\",
            \"kind\": \"function\",
            \"metadata\": {
                \"deprecation\": null,
                \"implementations\": {
                    \"gleam\": true,
                    \"uses_erlang_externals\": true,
                    \"uses_javascript_externals\": false
                }
            },
            \"json_signature\": {
                \"kind\": \"function\",
                \"name\": \"append\",
                \"return\": {
                    \"ref\": null,
                    \"kind\": \"named\",
                    \"name\": \"List\",
                    \"module\": \"gleam\",
                    \"package\": \"\",
                    \"parameters\": [
                        {
                            \"id\": 0,
                            \"kind\": \"variable\"
                        }
                    ]
                },
                \"parameters\": [
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": null,
                            \"kind\": \"named\",
                            \"name\": \"List\",
                            \"module\": \"gleam\",
                            \"package\": \"\",
                            \"parameters\": [
                                {
                                    \"id\": 0,
                                    \"kind\": \"variable\"
                                }
                            ]
                        },
                        \"label\": null
                    },
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": null,
                            \"kind\": \"named\",
                            \"name\": \"List\",
                            \"module\": \"gleam\",
                            \"package\": \"\",
                            \"parameters\": [
                                {
                                    \"id\": 0,
                                    \"kind\": \"variable\"
                                }
                            ]
                        },
                        \"label\": null
                    }
                ],
                \"deprecation\": \" Joins one list onto the end of another.\\n\\n This function runs in linear time, and it traverses and copies the first\\n list.\\n\\n ## Examples\\n\\n ```gleam\\n append([1, 2], [3])\\n // -> [1, 2, 3]\\n ```\\n\",
                \"documentation\": \" Joins one list onto the end of another.\\n\\n This function runs in linear time, and it traverses and copies the first\\n list.\\n\\n ## Examples\\n\\n ```gleam\\n append([1, 2], [3])\\n // -> [1, 2, 3]\\n ```\\n\",
                \"implementations\": {
                    \"gleam\": true,
                    \"uses_erlang_externals\": true,
                    \"uses_javascript_externals\": false
                }
            },
            \"module_name\": \"gleam/list\",
            \"package_name\": \"gleam_stdlib\",
            \"version\": \"0.37.0\"
        },
        {
            \"name\": \"append\",
            \"documentation\": \"Creates a new `String` by joining two `String`s together.\\n\\n This function copies both `String`s and runs in linear time. If you find\\n yourself joining `String`s frequently consider using the [`string_builder`](../gleam/string_builder.html)\\n module as it can append `String`s much faster!\\n\\n ## Examples\\n\\n ```gleam\\n append(to: \\\"butter\\\", suffix: \\\"fly\\\")\\n // -> \\\"butterfly\\\"\\n ```\",
            \"kind\": \"function\",
            \"metadata\": {
                \"deprecation\": null,
                \"implementations\": {
                    \"gleam\": false,
                    \"uses_erlang_externals\": true,
                    \"uses_javascript_externals\": true
                }
            },
            \"json_signature\": {
                \"kind\": \"function\",
                \"name\": \"append\",
                \"return\": {
                    \"ref\": null,
                    \"kind\": \"named\",
                    \"name\": \"String\",
                    \"module\": \"gleam\",
                    \"package\": \"\",
                    \"parameters\": []
                },
                \"parameters\": [
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": null,
                            \"kind\": \"named\",
                            \"name\": \"String\",
                            \"module\": \"gleam\",
                            \"package\": \"\",
                            \"parameters\": []
                        },
                        \"label\": \"to\"
                    },
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": null,
                            \"kind\": \"named\",
                            \"name\": \"String\",
                            \"module\": \"gleam\",
                            \"package\": \"\",
                            \"parameters\": []
                        },
                        \"label\": \"suffix\"
                    }
                ],
                \"deprecation\": \" Creates a new `String` by joining two `String`s together.\\n\\n This function copies both `String`s and runs in linear time. If you find\\n yourself joining `String`s frequently consider using the [`string_builder`](../gleam/string_builder.html)\\n module as it can append `String`s much faster!\\n\\n ## Examples\\n\\n ```gleam\\n append(to: \\\"butter\\\", suffix: \\\"fly\\\")\\n // -> \\\"butterfly\\\"\\n ```\\n\",
                \"documentation\": \" Creates a new `String` by joining two `String`s together.\\n\\n This function copies both `String`s and runs in linear time. If you find\\n yourself joining `String`s frequently consider using the [`string_builder`](../gleam/string_builder.html)\\n module as it can append `String`s much faster!\\n\\n ## Examples\\n\\n ```gleam\\n append(to: \\\"butter\\\", suffix: \\\"fly\\\")\\n // -> \\\"butterfly\\\"\\n ```\\n\",
                \"implementations\": {
                    \"gleam\": false,
                    \"uses_erlang_externals\": true,
                    \"uses_javascript_externals\": true
                }
            },
            \"module_name\": \"gleam/string\",
            \"package_name\": \"gleam_stdlib\",
            \"version\": \"0.37.0\"
        },
        {
            \"name\": \"append\",
            \"documentation\": \"Appends a `String` onto the end of some `StringBuilder`.\\n\\n Runs in constant time.\",
            \"kind\": \"function\",
            \"metadata\": {
                \"deprecation\": null,
                \"implementations\": {
                    \"gleam\": false,
                    \"uses_erlang_externals\": true,
                    \"uses_javascript_externals\": true
                }
            },
            \"json_signature\": {
                \"kind\": \"function\",
                \"name\": \"append\",
                \"return\": {
                    \"ref\": \"0.37.0\",
                    \"kind\": \"named\",
                    \"name\": \"StringBuilder\",
                    \"module\": \"gleam/string_builder\",
                    \"package\": \"gleam_stdlib\",
                    \"parameters\": []
                },
                \"parameters\": [
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": \"0.37.0\",
                            \"kind\": \"named\",
                            \"name\": \"StringBuilder\",
                            \"module\": \"gleam/string_builder\",
                            \"package\": \"gleam_stdlib\",
                            \"parameters\": []
                        },
                        \"label\": \"to\"
                    },
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": null,
                            \"kind\": \"named\",
                            \"name\": \"String\",
                            \"module\": \"gleam\",
                            \"package\": \"\",
                            \"parameters\": []
                        },
                        \"label\": \"suffix\"
                    }
                ],
                \"deprecation\": \" Appends a `String` onto the end of some `StringBuilder`.\\n\\n Runs in constant time.\\n\",
                \"documentation\": \" Appends a `String` onto the end of some `StringBuilder`.\\n\\n Runs in constant time.\\n\",
                \"implementations\": {
                    \"gleam\": false,
                    \"uses_erlang_externals\": true,
                    \"uses_javascript_externals\": true
                }
            },
            \"module_name\": \"gleam/string_builder\",
            \"package_name\": \"gleam_stdlib\",
            \"version\": \"0.37.0\"
        },
        {
            \"name\": \"append\",
            \"documentation\": \"Append to an existing Uri in explicit ways.\\n\\n ## Examples\\n\\n ```gleam\\n > import gleam/uri.Uri\\n >\\n > my_uri = Uri(Some(\\\"https\\\"), None, Some(\\\"example.com\\\"), Some(443), \\\"/the/path\\\", None, None)\\n > RelativePath(\\\"to/the/thing\\\") |> uri_append(to: my_uri)\\n \\n Uri(...my_uri, path: \\\"/the/path/to/the/thing\\\")\\n\\n > FullPath(\\\"to/the/thing\\\") |> uri_append(to: my_uri)\\n \\n Uri(...my_uri, path: \\\"to/the/thing\\\")\\n\\n > another_uri = Uri(Some(\\\"http\\\"), None, Some(\\\"localhost\\\"), Some(80), \\\"/over/here\\\", None, None)\\n > FullUri(another_uri) |> uri_append(to: my_uri)\\n\\n another_uri // <- it is the same as identity(another_uri)\",
            \"kind\": \"function\",
            \"metadata\": {
                \"deprecation\": null,
                \"implementations\": {
                    \"gleam\": false,
                    \"uses_erlang_externals\": true,
                    \"uses_javascript_externals\": true
                }
            },
            \"json_signature\": {
                \"kind\": \"function\",
                \"name\": \"append\",
                \"return\": {
                    \"ref\": \"0.37.0\",
                    \"kind\": \"named\",
                    \"name\": \"Uri\",
                    \"module\": \"gleam/uri\",
                    \"package\": \"gleam_stdlib\",
                    \"parameters\": []
                },
                \"parameters\": [
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": \"0.37.0\",
                            \"kind\": \"named\",
                            \"name\": \"Uri\",
                            \"module\": \"gleam/uri\",
                            \"package\": \"gleam_stdlib\",
                            \"parameters\": []
                        },
                        \"label\": \"to\"
                    },
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": \"0.4.0\",
                            \"kind\": \"named\",
                            \"name\": \"UriAppendage\",
                            \"module\": \"glow_auth/uri/uri_builder\",
                            \"package\": \"glow_auth\",
                            \"parameters\": []
                        },
                        \"label\": \"with\"
                    }
                ],
                \"deprecation\": \" Append to an existing Uri in explicit ways.\\n\\n ## Examples\\n\\n ```gleam\\n > import gleam/uri.Uri\\n >\\n > my_uri = Uri(Some(\\\"https\\\"), None, Some(\\\"example.com\\\"), Some(443), \\\"/the/path\\\", None, None)\\n > RelativePath(\\\"to/the/thing\\\") |> uri_append(to: my_uri)\\n \\n Uri(...my_uri, path: \\\"/the/path/to/the/thing\\\")\\n\\n > FullPath(\\\"to/the/thing\\\") |> uri_append(to: my_uri)\\n \\n Uri(...my_uri, path: \\\"to/the/thing\\\")\\n\\n > another_uri = Uri(Some(\\\"http\\\"), None, Some(\\\"localhost\\\"), Some(80), \\\"/over/here\\\", None, None)\\n > FullUri(another_uri) |> uri_append(to: my_uri)\\n\\n another_uri // <- it is the same as identity(another_uri)\\n\",
                \"documentation\": \" Append to an existing Uri in explicit ways.\\n\\n ## Examples\\n\\n ```gleam\\n > import gleam/uri.Uri\\n >\\n > my_uri = Uri(Some(\\\"https\\\"), None, Some(\\\"example.com\\\"), Some(443), \\\"/the/path\\\", None, None)\\n > RelativePath(\\\"to/the/thing\\\") |> uri_append(to: my_uri)\\n \\n Uri(...my_uri, path: \\\"/the/path/to/the/thing\\\")\\n\\n > FullPath(\\\"to/the/thing\\\") |> uri_append(to: my_uri)\\n \\n Uri(...my_uri, path: \\\"to/the/thing\\\")\\n\\n > another_uri = Uri(Some(\\\"http\\\"), None, Some(\\\"localhost\\\"), Some(80), \\\"/over/here\\\", None, None)\\n > FullUri(another_uri) |> uri_append(to: my_uri)\\n\\n another_uri // <- it is the same as identity(another_uri)\\n\",
                \"implementations\": {
                    \"gleam\": false,
                    \"uses_erlang_externals\": true,
                    \"uses_javascript_externals\": true
                }
            },
            \"module_name\": \"glow_auth/uri/uri_builder\",
            \"package_name\": \"glow_auth\",
            \"version\": \"0.4.0\"
        },
        {
            \"name\": \"append\",
            \"documentation\": \"Joins a non-empty list onto the end of a non-empty list.\\n\\n This function runs in linear time, and it traverses and copies the first non-empty list.\\n\\n ## Examples\\n\\n ```gleam\\n > new(1, [2, 3, 4])\\n > |> append(new(5, [6, 7]))\\n NonEmptyList(1, [2, 3, 4, 5, 6, 7])\\n ```\\n\\n ```gleam\\n > single(\\\"a\\\")\\n > |> append(new(\\\"b\\\", [\\\"c\\\"])\\n NonEmptyList(\\\"a\\\", [\\\"b\\\", \\\"c\\\"])\\n ````\",
            \"kind\": \"function\",
            \"metadata\": {
                \"deprecation\": null,
                \"implementations\": {
                    \"gleam\": true,
                    \"uses_erlang_externals\": true,
                    \"uses_javascript_externals\": false
                }
            },
            \"json_signature\": {
                \"kind\": \"function\",
                \"name\": \"append\",
                \"return\": {
                    \"ref\": \"2.0.0\",
                    \"kind\": \"named\",
                    \"name\": \"NonEmptyList\",
                    \"module\": \"non_empty_list\",
                    \"package\": \"non_empty_list\",
                    \"parameters\": [
                        {
                            \"id\": 0,
                            \"kind\": \"variable\"
                        }
                    ]
                },
                \"parameters\": [
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": \"2.0.0\",
                            \"kind\": \"named\",
                            \"name\": \"NonEmptyList\",
                            \"module\": \"non_empty_list\",
                            \"package\": \"non_empty_list\",
                            \"parameters\": [
                                {
                                    \"id\": 0,
                                    \"kind\": \"variable\"
                                }
                            ]
                        },
                        \"label\": null
                    },
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": \"2.0.0\",
                            \"kind\": \"named\",
                            \"name\": \"NonEmptyList\",
                            \"module\": \"non_empty_list\",
                            \"package\": \"non_empty_list\",
                            \"parameters\": [
                                {
                                    \"id\": 0,
                                    \"kind\": \"variable\"
                                }
                            ]
                        },
                        \"label\": null
                    }
                ],
                \"deprecation\": \" Joins a non-empty list onto the end of a non-empty list.\\n\\n This function runs in linear time, and it traverses and copies the first non-empty list.\\n\\n ## Examples\\n\\n ```gleam\\n > new(1, [2, 3, 4])\\n > |> append(new(5, [6, 7]))\\n NonEmptyList(1, [2, 3, 4, 5, 6, 7])\\n ```\\n\\n ```gleam\\n > single(\\\"a\\\")\\n > |> append(new(\\\"b\\\", [\\\"c\\\"])\\n NonEmptyList(\\\"a\\\", [\\\"b\\\", \\\"c\\\"])\\n ````\\n\",
                \"documentation\": \" Joins a non-empty list onto the end of a non-empty list.\\n\\n This function runs in linear time, and it traverses and copies the first non-empty list.\\n\\n ## Examples\\n\\n ```gleam\\n > new(1, [2, 3, 4])\\n > |> append(new(5, [6, 7]))\\n NonEmptyList(1, [2, 3, 4, 5, 6, 7])\\n ```\\n\\n ```gleam\\n > single(\\\"a\\\")\\n > |> append(new(\\\"b\\\", [\\\"c\\\"])\\n NonEmptyList(\\\"a\\\", [\\\"b\\\", \\\"c\\\"])\\n ````\\n\",
                \"implementations\": {
                    \"gleam\": true,
                    \"uses_erlang_externals\": true,
                    \"uses_javascript_externals\": false
                }
            },
            \"module_name\": \"non_empty_list\",
            \"package_name\": \"non_empty_list\",
            \"version\": \"2.0.0\"
        },
        {
            \"name\": \"append\",
            \"documentation\": \"see [here](https://redis.io/commands/append)!\",
            \"kind\": \"function\",
            \"metadata\": {
                \"deprecation\": null,
                \"implementations\": {
                    \"gleam\": false,
                    \"uses_erlang_externals\": true,
                    \"uses_javascript_externals\": true
                }
            },
            \"json_signature\": {
                \"kind\": \"function\",
                \"name\": \"append\",
                \"return\": {
                    \"ref\": null,
                    \"kind\": \"named\",
                    \"name\": \"Result\",
                    \"module\": \"gleam\",
                    \"package\": \"\",
                    \"parameters\": [
                        {
                            \"ref\": null,
                            \"kind\": \"named\",
                            \"name\": \"Int\",
                            \"module\": \"gleam\",
                            \"package\": \"\",
                            \"parameters\": []
                        },
                        {
                            \"ref\": \"0.15.0\",
                            \"kind\": \"named\",
                            \"name\": \"Error\",
                            \"module\": \"radish/error\",
                            \"package\": \"radish_fork\",
                            \"parameters\": []
                        }
                    ]
                },
                \"parameters\": [
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": \"0.25.0\",
                            \"kind\": \"named\",
                            \"name\": \"Subject\",
                            \"module\": \"gleam/erlang/process\",
                            \"package\": \"gleam_erlang\",
                            \"parameters\": [
                                {
                                    \"ref\": null,
                                    \"kind\": \"named\",
                                    \"name\": \"Message\",
                                    \"module\": \"radish/client\",
                                    \"package\": \"radish_fork\",
                                    \"parameters\": []
                                }
                            ]
                        },
                        \"label\": null
                    },
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": null,
                            \"kind\": \"named\",
                            \"name\": \"String\",
                            \"module\": \"gleam\",
                            \"package\": \"\",
                            \"parameters\": []
                        },
                        \"label\": null
                    },
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": null,
                            \"kind\": \"named\",
                            \"name\": \"String\",
                            \"module\": \"gleam\",
                            \"package\": \"\",
                            \"parameters\": []
                        },
                        \"label\": null
                    },
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": null,
                            \"kind\": \"named\",
                            \"name\": \"Int\",
                            \"module\": \"gleam\",
                            \"package\": \"\",
                            \"parameters\": []
                        },
                        \"label\": null
                    }
                ],
                \"deprecation\": \" see [here](https://redis.io/commands/append)!\",
                \"documentation\": \" see [here](https://redis.io/commands/append)!\",
                \"implementations\": {
                    \"gleam\": false,
                    \"uses_erlang_externals\": true,
                    \"uses_javascript_externals\": true
                }
            },
            \"module_name\": \"radish\",
            \"package_name\": \"radish_fork\",
            \"version\": \"0.15.0\"
        },
        {
            \"name\": \"append\",
            \"documentation\": \"Append a string to the contents of a file at the given path\\n ## Example\\n ```gleam\\n let assert Ok(Nil) = append(to: \\\"./needs_more_text.txt\\\", contents: \\\"more text\\\")\\n ```\",
            \"kind\": \"function\",
            \"metadata\": {
                \"deprecation\": null,
                \"implementations\": {
                    \"gleam\": false,
                    \"uses_erlang_externals\": true,
                    \"uses_javascript_externals\": true
                }
            },
            \"json_signature\": {
                \"kind\": \"function\",
                \"name\": \"append\",
                \"return\": {
                    \"ref\": null,
                    \"kind\": \"named\",
                    \"name\": \"Result\",
                    \"module\": \"gleam\",
                    \"package\": \"\",
                    \"parameters\": [
                        {
                            \"ref\": null,
                            \"kind\": \"named\",
                            \"name\": \"Nil\",
                            \"module\": \"gleam\",
                            \"package\": \"\",
                            \"parameters\": []
                        },
                        {
                            \"ref\": \"1.7.0\",
                            \"kind\": \"named\",
                            \"name\": \"FileError\",
                            \"module\": \"simplifile\",
                            \"package\": \"simplifile\",
                            \"parameters\": []
                        }
                    ]
                },
                \"parameters\": [
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": null,
                            \"kind\": \"named\",
                            \"name\": \"String\",
                            \"module\": \"gleam\",
                            \"package\": \"\",
                            \"parameters\": []
                        },
                        \"label\": \"to\"
                    },
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": null,
                            \"kind\": \"named\",
                            \"name\": \"String\",
                            \"module\": \"gleam\",
                            \"package\": \"\",
                            \"parameters\": []
                        },
                        \"label\": \"contents\"
                    }
                ],
                \"deprecation\": \" Append a string to the contents of a file at the given path\\n ## Example\\n ```gleam\\n let assert Ok(Nil) = append(to: \\\"./needs_more_text.txt\\\", contents: \\\"more text\\\")\\n ```\\n\",
                \"documentation\": \" Append a string to the contents of a file at the given path\\n ## Example\\n ```gleam\\n let assert Ok(Nil) = append(to: \\\"./needs_more_text.txt\\\", contents: \\\"more text\\\")\\n ```\\n\",
                \"implementations\": {
                    \"gleam\": false,
                    \"uses_erlang_externals\": true,
                    \"uses_javascript_externals\": true
                }
            },
            \"module_name\": \"simplifile\",
            \"package_name\": \"simplifile\",
            \"version\": \"1.7.0\"
        }
    ],
    \"matches\": [
        {
            \"name\": \"append_bits\",
            \"documentation\": \"Append a bitstring to the contents of a file at the given path\\n ## Example\\n ```gleam\\n let assert Ok(Nil) = append_bits(to: \\\"./needs_more_text.txt\\\", bits: <<\\\"more text\\\":utf8>>)\\n ```\",
            \"kind\": \"function\",
            \"metadata\": {
                \"deprecation\": null,
                \"implementations\": {
                    \"gleam\": false,
                    \"uses_erlang_externals\": true,
                    \"uses_javascript_externals\": false
                }
            },
            \"json_signature\": {
                \"kind\": \"function\",
                \"name\": \"append_bits\",
                \"return\": {
                    \"ref\": null,
                    \"kind\": \"named\",
                    \"name\": \"Result\",
                    \"module\": \"gleam\",
                    \"package\": \"\",
                    \"parameters\": [
                        {
                            \"ref\": null,
                            \"kind\": \"named\",
                            \"name\": \"Nil\",
                            \"module\": \"gleam\",
                            \"package\": \"\",
                            \"parameters\": []
                        },
                        {
                            \"ref\": \"1.7.0\",
                            \"kind\": \"named\",
                            \"name\": \"FileError\",
                            \"module\": \"simplifile\",
                            \"package\": \"simplifile\",
                            \"parameters\": []
                        }
                    ]
                },
                \"parameters\": [
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": null,
                            \"kind\": \"named\",
                            \"name\": \"String\",
                            \"module\": \"gleam\",
                            \"package\": \"\",
                            \"parameters\": []
                        },
                        \"label\": \"to\"
                    },
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": null,
                            \"kind\": \"named\",
                            \"name\": \"BitArray\",
                            \"module\": \"gleam\",
                            \"package\": \"\",
                            \"parameters\": []
                        },
                        \"label\": \"bits\"
                    }
                ],
                \"deprecation\": \" Append a bitstring to the contents of a file at the given path\\n ## Example\\n ```gleam\\n let assert Ok(Nil) = append_bits(to: \\\"./needs_more_text.txt\\\", bits: <<\\\"more text\\\":utf8>>)\\n ```\\n\",
                \"documentation\": \" Append a bitstring to the contents of a file at the given path\\n ## Example\\n ```gleam\\n let assert Ok(Nil) = append_bits(to: \\\"./needs_more_text.txt\\\", bits: <<\\\"more text\\\":utf8>>)\\n ```\\n\",
                \"implementations\": {
                    \"gleam\": false,
                    \"uses_erlang_externals\": true,
                    \"uses_javascript_externals\": false
                }
            },
            \"module_name\": \"simplifile\",
            \"package_name\": \"simplifile\",
            \"version\": \"1.7.0\"
        },
        {
            \"name\": \"append_builder\",
            \"documentation\": \"\",
            \"kind\": \"function\",
            \"metadata\": {
                \"deprecation\": \"Please use the `gleam/bytes_builder` module instead.\",
                \"implementations\": {
                    \"gleam\": true,
                    \"uses_erlang_externals\": true,
                    \"uses_javascript_externals\": false
                }
            },
            \"json_signature\": {
                \"kind\": \"function\",
                \"name\": \"append_builder\",
                \"return\": {
                    \"ref\": \"0.34.0\",
                    \"kind\": \"named\",
                    \"name\": \"BytesBuilder\",
                    \"module\": \"gleam/bytes_builder\",
                    \"package\": \"gleam_stdlib\",
                    \"parameters\": []
                },
                \"parameters\": [
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": \"0.34.0\",
                            \"kind\": \"named\",
                            \"name\": \"BytesBuilder\",
                            \"module\": \"gleam/bytes_builder\",
                            \"package\": \"gleam_stdlib\",
                            \"parameters\": []
                        },
                        \"label\": \"to\"
                    },
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": \"0.34.0\",
                            \"kind\": \"named\",
                            \"name\": \"BytesBuilder\",
                            \"module\": \"gleam/bytes_builder\",
                            \"package\": \"gleam_stdlib\",
                            \"parameters\": []
                        },
                        \"label\": \"suffix\"
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
            \"module_name\": \"gleam/bit_builder\",
            \"package_name\": \"gleam_stdlib\",
            \"version\": \"0.34.0\"
        },
        {
            \"name\": \"append_builder\",
            \"documentation\": \"Appends a builder onto the end of another.\\n\\n Runs in constant time.\",
            \"kind\": \"function\",
            \"metadata\": {
                \"deprecation\": null,
                \"implementations\": {
                    \"gleam\": true,
                    \"uses_erlang_externals\": true,
                    \"uses_javascript_externals\": false
                }
            },
            \"json_signature\": {
                \"kind\": \"function\",
                \"name\": \"append_builder\",
                \"return\": {
                    \"ref\": \"0.37.0\",
                    \"kind\": \"named\",
                    \"name\": \"BytesBuilder\",
                    \"module\": \"gleam/bytes_builder\",
                    \"package\": \"gleam_stdlib\",
                    \"parameters\": []
                },
                \"parameters\": [
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": \"0.37.0\",
                            \"kind\": \"named\",
                            \"name\": \"BytesBuilder\",
                            \"module\": \"gleam/bytes_builder\",
                            \"package\": \"gleam_stdlib\",
                            \"parameters\": []
                        },
                        \"label\": \"to\"
                    },
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": \"0.37.0\",
                            \"kind\": \"named\",
                            \"name\": \"BytesBuilder\",
                            \"module\": \"gleam/bytes_builder\",
                            \"package\": \"gleam_stdlib\",
                            \"parameters\": []
                        },
                        \"label\": \"suffix\"
                    }
                ],
                \"deprecation\": \" Appends a builder onto the end of another.\\n\\n Runs in constant time.\\n\",
                \"documentation\": \" Appends a builder onto the end of another.\\n\\n Runs in constant time.\\n\",
                \"implementations\": {
                    \"gleam\": true,
                    \"uses_erlang_externals\": true,
                    \"uses_javascript_externals\": false
                }
            },
            \"module_name\": \"gleam/bytes_builder\",
            \"package_name\": \"gleam_stdlib\",
            \"version\": \"0.37.0\"
        },
        {
            \"name\": \"append_builder\",
            \"documentation\": \"Appends some `StringBuilder` onto the end of another.\\n\\n Runs in constant time.\",
            \"kind\": \"function\",
            \"metadata\": {
                \"deprecation\": null,
                \"implementations\": {
                    \"gleam\": false,
                    \"uses_erlang_externals\": true,
                    \"uses_javascript_externals\": true
                }
            },
            \"json_signature\": {
                \"kind\": \"function\",
                \"name\": \"append_builder\",
                \"return\": {
                    \"ref\": \"0.37.0\",
                    \"kind\": \"named\",
                    \"name\": \"StringBuilder\",
                    \"module\": \"gleam/string_builder\",
                    \"package\": \"gleam_stdlib\",
                    \"parameters\": []
                },
                \"parameters\": [
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": \"0.37.0\",
                            \"kind\": \"named\",
                            \"name\": \"StringBuilder\",
                            \"module\": \"gleam/string_builder\",
                            \"package\": \"gleam_stdlib\",
                            \"parameters\": []
                        },
                        \"label\": \"to\"
                    },
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": \"0.37.0\",
                            \"kind\": \"named\",
                            \"name\": \"StringBuilder\",
                            \"module\": \"gleam/string_builder\",
                            \"package\": \"gleam_stdlib\",
                            \"parameters\": []
                        },
                        \"label\": \"suffix\"
                    }
                ],
                \"deprecation\": \" Appends some `StringBuilder` onto the end of another.\\n\\n Runs in constant time.\\n\",
                \"documentation\": \" Appends some `StringBuilder` onto the end of another.\\n\\n Runs in constant time.\\n\",
                \"implementations\": {
                    \"gleam\": false,
                    \"uses_erlang_externals\": true,
                    \"uses_javascript_externals\": true
                }
            },
            \"module_name\": \"gleam/string_builder\",
            \"package_name\": \"gleam_stdlib\",
            \"version\": \"0.37.0\"
        },
        {
            \"name\": \"append_child\",
            \"documentation\": \"\",
            \"kind\": \"function\",
            \"metadata\": {
                \"deprecation\": null,
                \"implementations\": {
                    \"gleam\": false,
                    \"uses_erlang_externals\": false,
                    \"uses_javascript_externals\": true
                }
            },
            \"json_signature\": {
                \"kind\": \"function\",
                \"name\": \"append_child\",
                \"return\": {
                    \"ref\": \"0.1.0\",
                    \"kind\": \"named\",
                    \"name\": \"HTMLElement\",
                    \"module\": \"glucose/dom\",
                    \"package\": \"glucose\",
                    \"parameters\": []
                },
                \"parameters\": [
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": \"0.1.0\",
                            \"kind\": \"named\",
                            \"name\": \"HTMLElement\",
                            \"module\": \"glucose/dom\",
                            \"package\": \"glucose\",
                            \"parameters\": []
                        },
                        \"label\": \"node\"
                    }
                ],
                \"deprecation\": null,
                \"documentation\": null,
                \"implementations\": {
                    \"gleam\": false,
                    \"uses_erlang_externals\": false,
                    \"uses_javascript_externals\": true
                }
            },
            \"module_name\": \"glucose/document\",
            \"package_name\": \"glucose\",
            \"version\": \"0.1.0\"
        },
        {
            \"name\": \"append_child\",
            \"documentation\": \"\",
            \"kind\": \"function\",
            \"metadata\": {
                \"deprecation\": null,
                \"implementations\": {
                    \"gleam\": false,
                    \"uses_erlang_externals\": false,
                    \"uses_javascript_externals\": true
                }
            },
            \"json_signature\": {
                \"kind\": \"function\",
                \"name\": \"append_child\",
                \"return\": {
                    \"ref\": null,
                    \"kind\": \"named\",
                    \"name\": \"Nil\",
                    \"module\": \"gleam\",
                    \"package\": \"\",
                    \"parameters\": []
                },
                \"parameters\": [
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": \"0.2.0\",
                            \"kind\": \"named\",
                            \"name\": \"Element\",
                            \"module\": \"plinth/browser/element\",
                            \"package\": \"plinth\",
                            \"parameters\": []
                        },
                        \"label\": null
                    },
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": \"0.2.0\",
                            \"kind\": \"named\",
                            \"name\": \"Element\",
                            \"module\": \"plinth/browser/element\",
                            \"package\": \"plinth\",
                            \"parameters\": []
                        },
                        \"label\": null
                    }
                ],
                \"deprecation\": null,
                \"documentation\": null,
                \"implementations\": {
                    \"gleam\": false,
                    \"uses_erlang_externals\": false,
                    \"uses_javascript_externals\": true
                }
            },
            \"module_name\": \"plinth/browser/element\",
            \"package_name\": \"plinth\",
            \"version\": \"0.2.0\"
        },
        {
            \"name\": \"append_child\",
            \"documentation\": \"\",
            \"kind\": \"function\",
            \"metadata\": {
                \"deprecation\": null,
                \"implementations\": {
                    \"gleam\": false,
                    \"uses_erlang_externals\": false,
                    \"uses_javascript_externals\": true
                }
            },
            \"json_signature\": {
                \"kind\": \"function\",
                \"name\": \"append_child\",
                \"return\": {
                    \"ref\": null,
                    \"kind\": \"named\",
                    \"name\": \"Nil\",
                    \"module\": \"gleam\",
                    \"package\": \"\",
                    \"parameters\": []
                },
                \"parameters\": [
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": \"0.2.0\",
                            \"kind\": \"named\",
                            \"name\": \"ShadowRoot\",
                            \"module\": \"plinth/browser/shadow\",
                            \"package\": \"plinth\",
                            \"parameters\": []
                        },
                        \"label\": null
                    },
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": \"0.2.0\",
                            \"kind\": \"named\",
                            \"name\": \"Element\",
                            \"module\": \"plinth/browser/element\",
                            \"package\": \"plinth\",
                            \"parameters\": []
                        },
                        \"label\": null
                    }
                ],
                \"deprecation\": null,
                \"documentation\": null,
                \"implementations\": {
                    \"gleam\": false,
                    \"uses_erlang_externals\": false,
                    \"uses_javascript_externals\": true
                }
            },
            \"module_name\": \"plinth/browser/shadow\",
            \"package_name\": \"plinth\",
            \"version\": \"0.2.0\"
        },
        {
            \"name\": \"append_docs\",
            \"documentation\": \"Joins multiple documents into the end of another.\\n\\n This is a shorthand for `append(to: first, doc: concat(docs))`.\\n\\n ## Examples\\n\\n ```gleam\\n from_string(\\\"pretty\\\")\\n |> append_docs([\\n   from_string(\\\"printing\\\"),\\n   space,\\n   from_string(\\\"rocks!\\\"),\\n ])\\n |> to_string(80)\\n // -> \\\"pretty printing rocks!\\\"\\n ```\",
            \"kind\": \"function\",
            \"metadata\": {
                \"deprecation\": null,
                \"implementations\": {
                    \"gleam\": true,
                    \"uses_erlang_externals\": true,
                    \"uses_javascript_externals\": false
                }
            },
            \"json_signature\": {
                \"kind\": \"function\",
                \"name\": \"append_docs\",
                \"return\": {
                    \"ref\": \"2.0.1\",
                    \"kind\": \"named\",
                    \"name\": \"Document\",
                    \"module\": \"glam/doc\",
                    \"package\": \"glam\",
                    \"parameters\": []
                },
                \"parameters\": [
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": \"2.0.1\",
                            \"kind\": \"named\",
                            \"name\": \"Document\",
                            \"module\": \"glam/doc\",
                            \"package\": \"glam\",
                            \"parameters\": []
                        },
                        \"label\": null
                    },
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": null,
                            \"kind\": \"named\",
                            \"name\": \"List\",
                            \"module\": \"gleam\",
                            \"package\": \"\",
                            \"parameters\": [
                                {
                                    \"ref\": \"2.0.1\",
                                    \"kind\": \"named\",
                                    \"name\": \"Document\",
                                    \"module\": \"glam/doc\",
                                    \"package\": \"glam\",
                                    \"parameters\": []
                                }
                            ]
                        },
                        \"label\": null
                    }
                ],
                \"deprecation\": \" Joins multiple documents into the end of another.\\n\\n This is a shorthand for `append(to: first, doc: concat(docs))`.\\n\\n ## Examples\\n\\n ```gleam\\n from_string(\\\"pretty\\\")\\n |> append_docs([\\n   from_string(\\\"printing\\\"),\\n   space,\\n   from_string(\\\"rocks!\\\"),\\n ])\\n |> to_string(80)\\n // -> \\\"pretty printing rocks!\\\"\\n ```\\n\",
                \"documentation\": \" Joins multiple documents into the end of another.\\n\\n This is a shorthand for `append(to: first, doc: concat(docs))`.\\n\\n ## Examples\\n\\n ```gleam\\n from_string(\\\"pretty\\\")\\n |> append_docs([\\n   from_string(\\\"printing\\\"),\\n   space,\\n   from_string(\\\"rocks!\\\"),\\n ])\\n |> to_string(80)\\n // -> \\\"pretty printing rocks!\\\"\\n ```\\n\",
                \"implementations\": {
                    \"gleam\": true,
                    \"uses_erlang_externals\": true,
                    \"uses_javascript_externals\": false
                }
            },
            \"module_name\": \"glam/doc\",
            \"package_name\": \"glam\",
            \"version\": \"2.0.1\"
        },
        {
            \"name\": \"append_list\",
            \"documentation\": \"Joins a list onto the end of a non-empty list.\\n\\n This function runs in linear time, and it traverses and copies the first non-empty list.\\n\\n ## Examples\\n\\n ```gleam\\n > new(1, [2, 3, 4])\\n > |> append_list([5, 6, 7])\\n NonEmptyList(1, [2, 3, 4, 5, 6, 7])\\n ```\\n\\n ```gleam\\n > new(\\\"a\\\", [\\\"b\\\", \\\"c\\\"])\\n > |> append_list([])\\n NonEmptyList(\\\"a\\\", [\\\"b\\\", \\\"c\\\"])\\n ```\",
            \"kind\": \"function\",
            \"metadata\": {
                \"deprecation\": null,
                \"implementations\": {
                    \"gleam\": true,
                    \"uses_erlang_externals\": true,
                    \"uses_javascript_externals\": false
                }
            },
            \"json_signature\": {
                \"kind\": \"function\",
                \"name\": \"append_list\",
                \"return\": {
                    \"ref\": \"2.0.0\",
                    \"kind\": \"named\",
                    \"name\": \"NonEmptyList\",
                    \"module\": \"non_empty_list\",
                    \"package\": \"non_empty_list\",
                    \"parameters\": [
                        {
                            \"id\": 0,
                            \"kind\": \"variable\"
                        }
                    ]
                },
                \"parameters\": [
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": \"2.0.0\",
                            \"kind\": \"named\",
                            \"name\": \"NonEmptyList\",
                            \"module\": \"non_empty_list\",
                            \"package\": \"non_empty_list\",
                            \"parameters\": [
                                {
                                    \"id\": 0,
                                    \"kind\": \"variable\"
                                }
                            ]
                        },
                        \"label\": null
                    },
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": null,
                            \"kind\": \"named\",
                            \"name\": \"List\",
                            \"module\": \"gleam\",
                            \"package\": \"\",
                            \"parameters\": [
                                {
                                    \"id\": 0,
                                    \"kind\": \"variable\"
                                }
                            ]
                        },
                        \"label\": null
                    }
                ],
                \"deprecation\": \" Joins a list onto the end of a non-empty list.\\n\\n This function runs in linear time, and it traverses and copies the first non-empty list.\\n\\n ## Examples\\n\\n ```gleam\\n > new(1, [2, 3, 4])\\n > |> append_list([5, 6, 7])\\n NonEmptyList(1, [2, 3, 4, 5, 6, 7])\\n ```\\n\\n ```gleam\\n > new(\\\"a\\\", [\\\"b\\\", \\\"c\\\"])\\n > |> append_list([])\\n NonEmptyList(\\\"a\\\", [\\\"b\\\", \\\"c\\\"])\\n ```\\n\",
                \"documentation\": \" Joins a list onto the end of a non-empty list.\\n\\n This function runs in linear time, and it traverses and copies the first non-empty list.\\n\\n ## Examples\\n\\n ```gleam\\n > new(1, [2, 3, 4])\\n > |> append_list([5, 6, 7])\\n NonEmptyList(1, [2, 3, 4, 5, 6, 7])\\n ```\\n\\n ```gleam\\n > new(\\\"a\\\", [\\\"b\\\", \\\"c\\\"])\\n > |> append_list([])\\n NonEmptyList(\\\"a\\\", [\\\"b\\\", \\\"c\\\"])\\n ```\\n\",
                \"implementations\": {
                    \"gleam\": true,
                    \"uses_erlang_externals\": true,
                    \"uses_javascript_externals\": false
                }
            },
            \"module_name\": \"non_empty_list\",
            \"package_name\": \"non_empty_list\",
            \"version\": \"2.0.0\"
        },
        {
            \"name\": \"append_path\",
            \"documentation\": \"\",
            \"kind\": \"function\",
            \"metadata\": {
                \"deprecation\": null,
                \"implementations\": {
                    \"gleam\": false,
                    \"uses_erlang_externals\": true,
                    \"uses_javascript_externals\": true
                }
            },
            \"json_signature\": {
                \"kind\": \"function\",
                \"name\": \"append_path\",
                \"return\": {
                    \"ref\": \"1.1.0\",
                    \"kind\": \"named\",
                    \"name\": \"Url\",
                    \"module\": \"falcon/core\",
                    \"package\": \"falcon\",
                    \"parameters\": []
                },
                \"parameters\": [
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": \"1.1.0\",
                            \"kind\": \"named\",
                            \"name\": \"Url\",
                            \"module\": \"falcon/core\",
                            \"package\": \"falcon\",
                            \"parameters\": []
                        },
                        \"label\": null
                    },
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": null,
                            \"kind\": \"named\",
                            \"name\": \"String\",
                            \"module\": \"gleam\",
                            \"package\": \"\",
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
            \"module_name\": \"falcon/core\",
            \"package_name\": \"falcon\",
            \"version\": \"1.1.0\"
        },
        {
            \"name\": \"append_path\",
            \"documentation\": \"Adds `dir` to the code path. The directory is added as the last directory in\\n the new path. If the directory already exists in the path, it is not added.\",
            \"kind\": \"function\",
            \"metadata\": {
                \"deprecation\": null,
                \"implementations\": {
                    \"gleam\": false,
                    \"uses_erlang_externals\": true,
                    \"uses_javascript_externals\": false
                }
            },
            \"json_signature\": {
                \"kind\": \"function\",
                \"name\": \"append_path\",
                \"return\": {
                    \"ref\": null,
                    \"kind\": \"named\",
                    \"name\": \"Result\",
                    \"module\": \"gleam\",
                    \"package\": \"\",
                    \"parameters\": [
                        {
                            \"ref\": null,
                            \"kind\": \"named\",
                            \"name\": \"Nil\",
                            \"module\": \"gleam\",
                            \"package\": \"\",
                            \"parameters\": []
                        },
                        {
                            \"ref\": \"1.1.0\",
                            \"kind\": \"named\",
                            \"name\": \"AddPathError\",
                            \"module\": \"glcode\",
                            \"package\": \"glcode\",
                            \"parameters\": []
                        }
                    ]
                },
                \"parameters\": [
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": null,
                            \"kind\": \"named\",
                            \"name\": \"String\",
                            \"module\": \"gleam\",
                            \"package\": \"\",
                            \"parameters\": []
                        },
                        \"label\": null
                    }
                ],
                \"deprecation\": \" Adds `dir` to the code path. The directory is added as the last directory in\\n the new path. If the directory already exists in the path, it is not added.\",
                \"documentation\": \" Adds `dir` to the code path. The directory is added as the last directory in\\n the new path. If the directory already exists in the path, it is not added.\",
                \"implementations\": {
                    \"gleam\": false,
                    \"uses_erlang_externals\": true,
                    \"uses_javascript_externals\": false
                }
            },
            \"module_name\": \"glcode\",
            \"package_name\": \"glcode\",
            \"version\": \"1.1.0\"
        },
        {
            \"name\": \"append_paths\",
            \"documentation\": \"Adds the directories in `dirs` to the end of the code path. If a already\\n directory exists, it is not added.\",
            \"kind\": \"function\",
            \"metadata\": {
                \"deprecation\": null,
                \"implementations\": {
                    \"gleam\": false,
                    \"uses_erlang_externals\": true,
                    \"uses_javascript_externals\": false
                }
            },
            \"json_signature\": {
                \"kind\": \"function\",
                \"name\": \"append_paths\",
                \"return\": {
                    \"ref\": null,
                    \"kind\": \"named\",
                    \"name\": \"Nil\",
                    \"module\": \"gleam\",
                    \"package\": \"\",
                    \"parameters\": []
                },
                \"parameters\": [
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": null,
                            \"kind\": \"named\",
                            \"name\": \"List\",
                            \"module\": \"gleam\",
                            \"package\": \"\",
                            \"parameters\": [
                                {
                                    \"ref\": null,
                                    \"kind\": \"named\",
                                    \"name\": \"String\",
                                    \"module\": \"gleam\",
                                    \"package\": \"\",
                                    \"parameters\": []
                                }
                            ]
                        },
                        \"label\": null
                    }
                ],
                \"deprecation\": \" Adds the directories in `dirs` to the end of the code path. If a already\\n directory exists, it is not added.\",
                \"documentation\": \" Adds the directories in `dirs` to the end of the code path. If a already\\n directory exists, it is not added.\",
                \"implementations\": {
                    \"gleam\": false,
                    \"uses_erlang_externals\": true,
                    \"uses_javascript_externals\": false
                }
            },
            \"module_name\": \"glcode\",
            \"package_name\": \"glcode\",
            \"version\": \"1.1.0\"
        },
        {
            \"name\": \"append_stage\",
            \"documentation\": \"\",
            \"kind\": \"function\",
            \"metadata\": {
                \"deprecation\": null,
                \"implementations\": {
                    \"gleam\": true,
                    \"uses_erlang_externals\": false,
                    \"uses_javascript_externals\": false
                }
            },
            \"json_signature\": {
                \"kind\": \"function\",
                \"name\": \"append_stage\",
                \"return\": {
                    \"ref\": \"0.26.0\",
                    \"kind\": \"named\",
                    \"name\": \"Pipeline\",
                    \"module\": \"mungo/aggregation\",
                    \"package\": \"mungo\",
                    \"parameters\": []
                },
                \"parameters\": [
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": \"0.26.0\",
                            \"kind\": \"named\",
                            \"name\": \"Pipeline\",
                            \"module\": \"mungo/aggregation\",
                            \"package\": \"mungo\",
                            \"parameters\": []
                        },
                        \"label\": null
                    },
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"kind\": \"tuple\",
                            \"elements\": [
                                {
                                    \"ref\": null,
                                    \"kind\": \"named\",
                                    \"name\": \"String\",
                                    \"module\": \"gleam\",
                                    \"package\": \"\",
                                    \"parameters\": []
                                },
                                {
                                    \"ref\": \"1.6.0\",
                                    \"kind\": \"named\",
                                    \"name\": \"Value\",
                                    \"module\": \"bison/bson\",
                                    \"package\": \"bison\",
                                    \"parameters\": []
                                }
                            ]
                        },
                        \"label\": null
                    }
                ],
                \"deprecation\": null,
                \"documentation\": null,
                \"implementations\": {
                    \"gleam\": true,
                    \"uses_erlang_externals\": false,
                    \"uses_javascript_externals\": false
                }
            },
            \"module_name\": \"mungo/aggregation\",
            \"package_name\": \"mungo\",
            \"version\": \"0.26.0\"
        },
        {
            \"name\": \"append_string\",
            \"documentation\": \"\",
            \"kind\": \"function\",
            \"metadata\": {
                \"deprecation\": \"Please use the `gleam/bytes_builder` module instead.\",
                \"implementations\": {
                    \"gleam\": false,
                    \"uses_erlang_externals\": true,
                    \"uses_javascript_externals\": true
                }
            },
            \"json_signature\": {
                \"kind\": \"function\",
                \"name\": \"append_string\",
                \"return\": {
                    \"ref\": \"0.34.0\",
                    \"kind\": \"named\",
                    \"name\": \"BytesBuilder\",
                    \"module\": \"gleam/bytes_builder\",
                    \"package\": \"gleam_stdlib\",
                    \"parameters\": []
                },
                \"parameters\": [
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": \"0.34.0\",
                            \"kind\": \"named\",
                            \"name\": \"BytesBuilder\",
                            \"module\": \"gleam/bytes_builder\",
                            \"package\": \"gleam_stdlib\",
                            \"parameters\": []
                        },
                        \"label\": null
                    },
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": null,
                            \"kind\": \"named\",
                            \"name\": \"String\",
                            \"module\": \"gleam\",
                            \"package\": \"\",
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
            \"module_name\": \"gleam/bit_builder\",
            \"package_name\": \"gleam_stdlib\",
            \"version\": \"0.34.0\"
        },
        {
            \"name\": \"append_string\",
            \"documentation\": \"Appends a string onto the end of a builder.\\n\\n Runs in constant time when running on Erlang.\\n Runs in linear time with the length of the string otherwise.\",
            \"kind\": \"function\",
            \"metadata\": {
                \"deprecation\": null,
                \"implementations\": {
                    \"gleam\": false,
                    \"uses_erlang_externals\": true,
                    \"uses_javascript_externals\": true
                }
            },
            \"json_signature\": {
                \"kind\": \"function\",
                \"name\": \"append_string\",
                \"return\": {
                    \"ref\": \"0.37.0\",
                    \"kind\": \"named\",
                    \"name\": \"BytesBuilder\",
                    \"module\": \"gleam/bytes_builder\",
                    \"package\": \"gleam_stdlib\",
                    \"parameters\": []
                },
                \"parameters\": [
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": \"0.37.0\",
                            \"kind\": \"named\",
                            \"name\": \"BytesBuilder\",
                            \"module\": \"gleam/bytes_builder\",
                            \"package\": \"gleam_stdlib\",
                            \"parameters\": []
                        },
                        \"label\": \"to\"
                    },
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": null,
                            \"kind\": \"named\",
                            \"name\": \"String\",
                            \"module\": \"gleam\",
                            \"package\": \"\",
                            \"parameters\": []
                        },
                        \"label\": \"suffix\"
                    }
                ],
                \"deprecation\": \" Appends a string onto the end of a builder.\\n\\n Runs in constant time when running on Erlang.\\n Runs in linear time with the length of the string otherwise.\\n\",
                \"documentation\": \" Appends a string onto the end of a builder.\\n\\n Runs in constant time when running on Erlang.\\n Runs in linear time with the length of the string otherwise.\\n\",
                \"implementations\": {
                    \"gleam\": false,
                    \"uses_erlang_externals\": true,
                    \"uses_javascript_externals\": true
                }
            },
            \"module_name\": \"gleam/bytes_builder\",
            \"package_name\": \"gleam_stdlib\",
            \"version\": \"0.37.0\"
        },
        {
            \"name\": \"append_string\",
            \"documentation\": \"\",
            \"kind\": \"function\",
            \"metadata\": {
                \"deprecation\": null,
                \"implementations\": {
                    \"gleam\": false,
                    \"uses_erlang_externals\": true,
                    \"uses_javascript_externals\": true
                }
            },
            \"json_signature\": {
                \"kind\": \"function\",
                \"name\": \"append_string\",
                \"return\": {
                    \"ref\": \"0.1.0\",
                    \"kind\": \"named\",
                    \"name\": \"Path\",
                    \"module\": \"gleam_community/path\",
                    \"package\": \"gleam_community_path\",
                    \"parameters\": []
                },
                \"parameters\": [
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": \"0.1.0\",
                            \"kind\": \"named\",
                            \"name\": \"Path\",
                            \"module\": \"gleam_community/path\",
                            \"package\": \"gleam_community_path\",
                            \"parameters\": []
                        },
                        \"label\": null
                    },
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": null,
                            \"kind\": \"named\",
                            \"name\": \"String\",
                            \"module\": \"gleam\",
                            \"package\": \"\",
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
            \"module_name\": \"gleam_community/path\",
            \"package_name\": \"gleam_community_path\",
            \"version\": \"0.1.0\"
        },
        {
            \"name\": \"authorization_code\",
            \"documentation\": \"Build a token request using a code in \\n [Authorization Code grant](https://datatracker.ietf.org/doc/html/rfc6749#section-4.1.3).\\n\\n Note that the redirect_uri must be identical to usage in the\\n [Authorization Uri](./authorize_uri.html).\",
            \"kind\": \"function\",
            \"metadata\": {
                \"deprecation\": null,
                \"implementations\": {
                    \"gleam\": false,
                    \"uses_erlang_externals\": true,
                    \"uses_javascript_externals\": true
                }
            },
            \"json_signature\": {
                \"kind\": \"function\",
                \"name\": \"authorization_code\",
                \"return\": {
                    \"ref\": \"3.6.0\",
                    \"kind\": \"named\",
                    \"name\": \"Request\",
                    \"module\": \"gleam/http/request\",
                    \"package\": \"gleam_http\",
                    \"parameters\": [
                        {
                            \"ref\": null,
                            \"kind\": \"named\",
                            \"name\": \"String\",
                            \"module\": \"gleam\",
                            \"package\": \"\",
                            \"parameters\": []
                        }
                    ]
                },
                \"parameters\": [
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": \"0.4.0\",
                            \"kind\": \"named\",
                            \"name\": \"Client\",
                            \"module\": \"glow_auth\",
                            \"package\": \"glow_auth\",
                            \"parameters\": [
                                {
                                    \"id\": 0,
                                    \"kind\": \"variable\"
                                }
                            ]
                        },
                        \"label\": null
                    },
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": \"0.4.0\",
                            \"kind\": \"named\",
                            \"name\": \"UriAppendage\",
                            \"module\": \"glow_auth/uri/uri_builder\",
                            \"package\": \"glow_auth\",
                            \"parameters\": []
                        },
                        \"label\": null
                    },
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": null,
                            \"kind\": \"named\",
                            \"name\": \"String\",
                            \"module\": \"gleam\",
                            \"package\": \"\",
                            \"parameters\": []
                        },
                        \"label\": null
                    },
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": \"0.37.0\",
                            \"kind\": \"named\",
                            \"name\": \"Uri\",
                            \"module\": \"gleam/uri\",
                            \"package\": \"gleam_stdlib\",
                            \"parameters\": []
                        },
                        \"label\": null
                    }
                ],
                \"deprecation\": \" Build a token request using a code in \\n [Authorization Code grant](https://datatracker.ietf.org/doc/html/rfc6749#section-4.1.3).\\n\\n Note that the redirect_uri must be identical to usage in the\\n [Authorization Uri](./authorize_uri.html).\",
                \"documentation\": \" Build a token request using a code in \\n [Authorization Code grant](https://datatracker.ietf.org/doc/html/rfc6749#section-4.1.3).\\n\\n Note that the redirect_uri must be identical to usage in the\\n [Authorization Uri](./authorize_uri.html).\",
                \"implementations\": {
                    \"gleam\": false,
                    \"uses_erlang_externals\": true,
                    \"uses_javascript_externals\": true
                }
            },
            \"module_name\": \"glow_auth/token_request\",
            \"package_name\": \"glow_auth\",
            \"version\": \"0.4.0\"
        },
        {
            \"name\": \"AuthUriSpec\",
            \"documentation\": \"Represents the details needed to build an authorization Uri.\\n\\n Use [build](#build), [set_scope](#set_scope), [set_state](#set_state) to build\\n up one of these, then [to_code_authorization_uri](#to_code_authorization_uri)\\n or [to_implicit_authorization_uri](#to_implicit_authorization_uri)\\n to convert to a Uri.\",
            \"kind\": \"type_definition\",
            \"metadata\": {
                \"deprecation\": null
            },
            \"json_signature\": {
                \"kind\": \"type-definition\",
                \"name\": \"AuthUriSpec\",
                \"parameters\": 1,
                \"deprecation\": \" Represents the details needed to build an authorization Uri.\\n\\n Use [build](#build), [set_scope](#set_scope), [set_state](#set_state) to build\\n up one of these, then [to_code_authorization_uri](#to_code_authorization_uri)\\n or [to_implicit_authorization_uri](#to_implicit_authorization_uri)\\n to convert to a Uri.\",
                \"constructors\": [
                    {
                        \"kind\": \"type-constructor\",
                        \"name\": \"AuthUriSpec\",
                        \"parameters\": [
                            {
                                \"kind\": \"parameter\",
                                \"type\": {
                                    \"ref\": \"0.4.0\",
                                    \"kind\": \"named\",
                                    \"name\": \"Client\",
                                    \"module\": \"glow_auth\",
                                    \"package\": \"glow_auth\",
                                    \"parameters\": [
                                        {
                                            \"id\": 0,
                                            \"kind\": \"variable\"
                                        }
                                    ]
                                },
                                \"label\": \"client\"
                            },
                            {
                                \"kind\": \"parameter\",
                                \"type\": {
                                    \"ref\": \"0.4.0\",
                                    \"kind\": \"named\",
                                    \"name\": \"UriAppendage\",
                                    \"module\": \"glow_auth/uri/uri_builder\",
                                    \"package\": \"glow_auth\",
                                    \"parameters\": []
                                },
                                \"label\": \"authorize_uri\"
                            },
                            {
                                \"kind\": \"parameter\",
                                \"type\": {
                                    \"ref\": \"0.37.0\",
                                    \"kind\": \"named\",
                                    \"name\": \"Uri\",
                                    \"module\": \"gleam/uri\",
                                    \"package\": \"gleam_stdlib\",
                                    \"parameters\": []
                                },
                                \"label\": \"redirect_uri\"
                            },
                            {
                                \"kind\": \"parameter\",
                                \"type\": {
                                    \"ref\": \"0.37.0\",
                                    \"kind\": \"named\",
                                    \"name\": \"Option\",
                                    \"module\": \"gleam/option\",
                                    \"package\": \"gleam_stdlib\",
                                    \"parameters\": [
                                        {
                                            \"ref\": null,
                                            \"kind\": \"named\",
                                            \"name\": \"String\",
                                            \"module\": \"gleam\",
                                            \"package\": \"\",
                                            \"parameters\": []
                                        }
                                    ]
                                },
                                \"label\": \"scope\"
                            },
                            {
                                \"kind\": \"parameter\",
                                \"type\": {
                                    \"ref\": \"0.37.0\",
                                    \"kind\": \"named\",
                                    \"name\": \"Option\",
                                    \"module\": \"gleam/option\",
                                    \"package\": \"gleam_stdlib\",
                                    \"parameters\": [
                                        {
                                            \"ref\": null,
                                            \"kind\": \"named\",
                                            \"name\": \"String\",
                                            \"module\": \"gleam\",
                                            \"package\": \"\",
                                            \"parameters\": []
                                        }
                                    ]
                                },
                                \"label\": \"state\"
                            }
                        ],
                        \"documentation\": null
                    }
                ],
                \"documentation\": \" Represents the details needed to build an authorization Uri.\\n\\n Use [build](#build), [set_scope](#set_scope), [set_state](#set_state) to build\\n up one of these, then [to_code_authorization_uri](#to_code_authorization_uri)\\n or [to_implicit_authorization_uri](#to_implicit_authorization_uri)\\n to convert to a Uri.\"
            },
            \"module_name\": \"glow_auth/authorize_uri\",
            \"package_name\": \"glow_auth\",
            \"version\": \"0.4.0\"
        },
        {
            \"name\": \"build\",
            \"documentation\": \"Build a AuthUriSpec for an AuthCode authorize_uri.\\n\\n Important things to note:\\n  * The exact redirect_uri specified in this uri must also be provided\\n    when requesting an access token.\",
            \"kind\": \"function\",
            \"metadata\": {
                \"deprecation\": null,
                \"implementations\": {
                    \"gleam\": true,
                    \"uses_erlang_externals\": false,
                    \"uses_javascript_externals\": false
                }
            },
            \"json_signature\": {
                \"kind\": \"function\",
                \"name\": \"build\",
                \"return\": {
                    \"ref\": \"0.4.0\",
                    \"kind\": \"named\",
                    \"name\": \"AuthUriSpec\",
                    \"module\": \"glow_auth/authorize_uri\",
                    \"package\": \"glow_auth\",
                    \"parameters\": [
                        {
                            \"id\": 0,
                            \"kind\": \"variable\"
                        }
                    ]
                },
                \"parameters\": [
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": \"0.4.0\",
                            \"kind\": \"named\",
                            \"name\": \"Client\",
                            \"module\": \"glow_auth\",
                            \"package\": \"glow_auth\",
                            \"parameters\": [
                                {
                                    \"id\": 0,
                                    \"kind\": \"variable\"
                                }
                            ]
                        },
                        \"label\": null
                    },
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": \"0.4.0\",
                            \"kind\": \"named\",
                            \"name\": \"UriAppendage\",
                            \"module\": \"glow_auth/uri/uri_builder\",
                            \"package\": \"glow_auth\",
                            \"parameters\": []
                        },
                        \"label\": null
                    },
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": \"0.37.0\",
                            \"kind\": \"named\",
                            \"name\": \"Uri\",
                            \"module\": \"gleam/uri\",
                            \"package\": \"gleam_stdlib\",
                            \"parameters\": []
                        },
                        \"label\": null
                    }
                ],
                \"deprecation\": \" Build a AuthUriSpec for an AuthCode authorize_uri.\\n\\n Important things to note:\\n  * The exact redirect_uri specified in this uri must also be provided\\n    when requesting an access token.\",
                \"documentation\": \" Build a AuthUriSpec for an AuthCode authorize_uri.\\n\\n Important things to note:\\n  * The exact redirect_uri specified in this uri must also be provided\\n    when requesting an access token.\",
                \"implementations\": {
                    \"gleam\": true,
                    \"uses_erlang_externals\": false,
                    \"uses_javascript_externals\": false
                }
            },
            \"module_name\": \"glow_auth/authorize_uri\",
            \"package_name\": \"glow_auth\",
            \"version\": \"0.4.0\"
        },
        {
            \"name\": \"client_credentials\",
            \"documentation\": \"Build a token request using just the client id/secret in\\n [Client Credentials grant](https://datatracker.ietf.org/doc/html/rfc6749#section-4.4.2)\",
            \"kind\": \"function\",
            \"metadata\": {
                \"deprecation\": null,
                \"implementations\": {
                    \"gleam\": false,
                    \"uses_erlang_externals\": true,
                    \"uses_javascript_externals\": true
                }
            },
            \"json_signature\": {
                \"kind\": \"function\",
                \"name\": \"client_credentials\",
                \"return\": {
                    \"ref\": \"3.6.0\",
                    \"kind\": \"named\",
                    \"name\": \"Request\",
                    \"module\": \"gleam/http/request\",
                    \"package\": \"gleam_http\",
                    \"parameters\": [
                        {
                            \"ref\": null,
                            \"kind\": \"named\",
                            \"name\": \"String\",
                            \"module\": \"gleam\",
                            \"package\": \"\",
                            \"parameters\": []
                        }
                    ]
                },
                \"parameters\": [
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": \"0.4.0\",
                            \"kind\": \"named\",
                            \"name\": \"Client\",
                            \"module\": \"glow_auth\",
                            \"package\": \"glow_auth\",
                            \"parameters\": [
                                {
                                    \"id\": 0,
                                    \"kind\": \"variable\"
                                }
                            ]
                        },
                        \"label\": null
                    },
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": \"0.4.0\",
                            \"kind\": \"named\",
                            \"name\": \"UriAppendage\",
                            \"module\": \"glow_auth/uri/uri_builder\",
                            \"package\": \"glow_auth\",
                            \"parameters\": []
                        },
                        \"label\": null
                    },
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": \"0.4.0\",
                            \"kind\": \"named\",
                            \"name\": \"AuthScheme\",
                            \"module\": \"glow_auth/token_request\",
                            \"package\": \"glow_auth\",
                            \"parameters\": []
                        },
                        \"label\": null
                    }
                ],
                \"deprecation\": \" Build a token request using just the client id/secret in\\n [Client Credentials grant](https://datatracker.ietf.org/doc/html/rfc6749#section-4.4.2)\",
                \"documentation\": \" Build a token request using just the client id/secret in\\n [Client Credentials grant](https://datatracker.ietf.org/doc/html/rfc6749#section-4.4.2)\",
                \"implementations\": {
                    \"gleam\": false,
                    \"uses_erlang_externals\": true,
                    \"uses_javascript_externals\": true
                }
            },
            \"module_name\": \"glow_auth/token_request\",
            \"package_name\": \"glow_auth\",
            \"version\": \"0.4.0\"
        },
        {
            \"name\": \"Config\",
            \"documentation\": \"\",
            \"kind\": \"type_definition\",
            \"metadata\": {
                \"deprecation\": null
            },
            \"json_signature\": {
                \"kind\": \"type-definition\",
                \"name\": \"Config\",
                \"parameters\": 0,
                \"deprecation\": null,
                \"constructors\": [
                    {
                        \"kind\": \"type-constructor\",
                        \"name\": \"Body\",
                        \"parameters\": [
                            {
                                \"kind\": \"parameter\",
                                \"type\": {
                                    \"ref\": \"1.1.0\",
                                    \"kind\": \"named\",
                                    \"name\": \"OpaqueBody\",
                                    \"module\": \"falcon/core\",
                                    \"package\": \"falcon\",
                                    \"parameters\": []
                                },
                                \"label\": null
                            }
                        ],
                        \"documentation\": \" A string to use as the body of the request - not meant to be used directly, use `post`, `put`, or `patch` instead, or if necessary, `to_body`\"
                    },
                    {
                        \"kind\": \"type-constructor\",
                        \"name\": \"Headers\",
                        \"parameters\": [
                            {
                                \"kind\": \"parameter\",
                                \"type\": {
                                    \"ref\": null,
                                    \"kind\": \"named\",
                                    \"name\": \"List\",
                                    \"module\": \"gleam\",
                                    \"package\": \"\",
                                    \"parameters\": [
                                        {
                                            \"kind\": \"tuple\",
                                            \"elements\": [
                                                {
                                                    \"ref\": null,
                                                    \"kind\": \"named\",
                                                    \"name\": \"String\",
                                                    \"module\": \"gleam\",
                                                    \"package\": \"\",
                                                    \"parameters\": []
                                                },
                                                {
                                                    \"ref\": null,
                                                    \"kind\": \"named\",
                                                    \"name\": \"String\",
                                                    \"module\": \"gleam\",
                                                    \"package\": \"\",
                                                    \"parameters\": []
                                                }
                                            ]
                                        }
                                    ]
                                },
                                \"label\": null
                            }
                        ],
                        \"documentation\": \" A list of headers to *prepend* to the request\"
                    },
                    {
                        \"kind\": \"type-constructor\",
                        \"name\": \"Queries\",
                        \"parameters\": [
                            {
                                \"kind\": \"parameter\",
                                \"type\": {
                                    \"ref\": null,
                                    \"kind\": \"named\",
                                    \"name\": \"List\",
                                    \"module\": \"gleam\",
                                    \"package\": \"\",
                                    \"parameters\": [
                                        {
                                            \"kind\": \"tuple\",
                                            \"elements\": [
                                                {
                                                    \"ref\": null,
                                                    \"kind\": \"named\",
                                                    \"name\": \"String\",
                                                    \"module\": \"gleam\",
                                                    \"package\": \"\",
                                                    \"parameters\": []
                                                },
                                                {
                                                    \"ref\": null,
                                                    \"kind\": \"named\",
                                                    \"name\": \"String\",
                                                    \"module\": \"gleam\",
                                                    \"package\": \"\",
                                                    \"parameters\": []
                                                }
                                            ]
                                        }
                                    ]
                                },
                                \"label\": null
                            }
                        ],
                        \"documentation\": \" A list of query parameters to append to the URL\"
                    },
                    {
                        \"kind\": \"type-constructor\",
                        \"name\": \"ClientOptions\",
                        \"parameters\": [
                            {
                                \"kind\": \"parameter\",
                                \"type\": {
                                    \"ref\": null,
                                    \"kind\": \"named\",
                                    \"name\": \"List\",
                                    \"module\": \"gleam\",
                                    \"package\": \"\",
                                    \"parameters\": [
                                        {
                                            \"ref\": \"1.1.0\",
                                            \"kind\": \"named\",
                                            \"name\": \"HackneyOption\",
                                            \"module\": \"falcon/hackney\",
                                            \"package\": \"falcon\",
                                            \"parameters\": []
                                        }
                                    ]
                                },
                                \"label\": null
                            }
                        ],
                        \"documentation\": \" A list of options to pass to the underlying HTTP client to control things like timeouts and redirects\"
                    }
                ],
                \"documentation\": null
            },
            \"module_name\": \"falcon/core\",
            \"package_name\": \"falcon\",
            \"version\": \"1.1.0\"
        },
        {
            \"name\": \"DiscardPolicy\",
            \"documentation\": \"Used to set the discard policy of a stream.\",
            \"kind\": \"type_definition\",
            \"metadata\": {
                \"deprecation\": null
            },
            \"json_signature\": {
                \"kind\": \"type-definition\",
                \"name\": \"DiscardPolicy\",
                \"parameters\": 0,
                \"deprecation\": \" Used to set the discard policy of a stream.\\n\",
                \"constructors\": [
                    {
                        \"kind\": \"type-constructor\",
                        \"name\": \"DiscardOld\",
                        \"parameters\": [],
                        \"documentation\": \" This policy will delete the oldest messages in order\\n to maintain the limit. For example, if `MaxAge` is set\\n to one minute, the server will automatically delete\\n messages older than one minute with this policy.\"
                    },
                    {
                        \"kind\": \"type-constructor\",
                        \"name\": \"DiscardNew\",
                        \"parameters\": [],
                        \"documentation\": \" This policy will reject new messages from being\\n appended to the stream if it would exceed one of the\\n limits. An extension to this policy is\\n `DiscardNewPerSubject` which will apply this policy\\n on a per-subject basis within the stream.\"
                    }
                ],
                \"documentation\": \" Used to set the discard policy of a stream.\\n\"
            },
            \"module_name\": \"glats/jetstream/stream\",
            \"package_name\": \"glats\",
            \"version\": \"0.7.0\"
        },
        {
            \"name\": \"Part\",
            \"documentation\": \"`Part` is used to indicate to a custom serializer if it should produce a serialization\\n based on a segment with items or the final string that contains already serialized segments\",
            \"kind\": \"type_definition\",
            \"metadata\": {
                \"deprecation\": null
            },
            \"json_signature\": {
                \"kind\": \"type-definition\",
                \"name\": \"Part\",
                \"parameters\": 1,
                \"deprecation\": \" `Part` is used to indicate to a custom serializer if it should produce a serialization\\n based on a segment with items or the final string that contains already serialized segments\",
                \"constructors\": [
                    {
                        \"kind\": \"type-constructor\",
                        \"name\": \"Part\",
                        \"parameters\": [
                            {
                                \"kind\": \"parameter\",
                                \"type\": {
                                    \"ref\": null,
                                    \"kind\": \"named\",
                                    \"name\": \"String\",
                                    \"module\": \"gleam\",
                                    \"package\": \"\",
                                    \"parameters\": []
                                },
                                \"label\": \"acc\"
                            },
                            {
                                \"kind\": \"parameter\",
                                \"type\": {
                                    \"ref\": null,
                                    \"kind\": \"named\",
                                    \"name\": \"List\",
                                    \"module\": \"gleam\",
                                    \"package\": \"\",
                                    \"parameters\": [
                                        {
                                            \"id\": 0,
                                            \"kind\": \"variable\"
                                        }
                                    ]
                                },
                                \"label\": \"part\"
                            },
                            {
                                \"kind\": \"parameter\",
                                \"type\": {
                                    \"kind\": \"fn\",
                                    \"params\": [
                                        {
                                            \"ref\": null,
                                            \"kind\": \"named\",
                                            \"name\": \"String\",
                                            \"module\": \"gleam\",
                                            \"package\": \"\",
                                            \"parameters\": []
                                        }
                                    ],
                                    \"return\": {
                                        \"ref\": null,
                                        \"kind\": \"named\",
                                        \"name\": \"String\",
                                        \"module\": \"gleam\",
                                        \"package\": \"\",
                                        \"parameters\": []
                                    }
                                },
                                \"label\": \"highlight\"
                            }
                        ],
                        \"documentation\": \" `acc` the already serialized part of the result, `part` is the current segment that should be serialized and appended and `highlighter` is the `Highlighter` that can be used to indicate non-matching items\"
                    },
                    {
                        \"kind\": \"type-constructor\",
                        \"name\": \"All\",
                        \"parameters\": [
                            {
                                \"kind\": \"parameter\",
                                \"type\": {
                                    \"ref\": null,
                                    \"kind\": \"named\",
                                    \"name\": \"String\",
                                    \"module\": \"gleam\",
                                    \"package\": \"\",
                                    \"parameters\": []
                                },
                                \"label\": \"all\"
                            }
                        ],
                        \"documentation\": \" `all` is a string representing all serialized segments. This can be useful if some string should be prepended/appended to the final result\"
                    }
                ],
                \"documentation\": \" `Part` is used to indicate to a custom serializer if it should produce a serialization\\n based on a segment with items or the final string that contains already serialized segments\"
            },
            \"module_name\": \"gap/styling\",
            \"package_name\": \"gap\",
            \"version\": \"1.1.3\"
        },
        {
            \"name\": \"QueryError\",
            \"documentation\": \"\",
            \"kind\": \"type_definition\",
            \"metadata\": {
                \"deprecation\": null
            },
            \"json_signature\": {
                \"kind\": \"type-definition\",
                \"name\": \"QueryError\",
                \"parameters\": 0,
                \"deprecation\": null,
                \"constructors\": [
                    {
                        \"kind\": \"type-constructor\",
                        \"name\": \"ConstraintViolated\",
                        \"parameters\": [
                            {
                                \"kind\": \"parameter\",
                                \"type\": {
                                    \"ref\": null,
                                    \"kind\": \"named\",
                                    \"name\": \"String\",
                                    \"module\": \"gleam\",
                                    \"package\": \"\",
                                    \"parameters\": []
                                },
                                \"label\": \"message\"
                            },
                            {
                                \"kind\": \"parameter\",
                                \"type\": {
                                    \"ref\": null,
                                    \"kind\": \"named\",
                                    \"name\": \"String\",
                                    \"module\": \"gleam\",
                                    \"package\": \"\",
                                    \"parameters\": []
                                },
                                \"label\": \"constraint\"
                            },
                            {
                                \"kind\": \"parameter\",
                                \"type\": {
                                    \"ref\": null,
                                    \"kind\": \"named\",
                                    \"name\": \"String\",
                                    \"module\": \"gleam\",
                                    \"package\": \"\",
                                    \"parameters\": []
                                },
                                \"label\": \"detail\"
                            }
                        ],
                        \"documentation\": \" The query failed as a database constraint would have been violated by the\\n change.\"
                    },
                    {
                        \"kind\": \"type-constructor\",
                        \"name\": \"PostgresqlError\",
                        \"parameters\": [
                            {
                                \"kind\": \"parameter\",
                                \"type\": {
                                    \"ref\": null,
                                    \"kind\": \"named\",
                                    \"name\": \"String\",
                                    \"module\": \"gleam\",
                                    \"package\": \"\",
                                    \"parameters\": []
                                },
                                \"label\": \"code\"
                            },
                            {
                                \"kind\": \"parameter\",
                                \"type\": {
                                    \"ref\": null,
                                    \"kind\": \"named\",
                                    \"name\": \"String\",
                                    \"module\": \"gleam\",
                                    \"package\": \"\",
                                    \"parameters\": []
                                },
                                \"label\": \"name\"
                            },
                            {
                                \"kind\": \"parameter\",
                                \"type\": {
                                    \"ref\": null,
                                    \"kind\": \"named\",
                                    \"name\": \"String\",
                                    \"module\": \"gleam\",
                                    \"package\": \"\",
                                    \"parameters\": []
                                },
                                \"label\": \"message\"
                            }
                        ],
                        \"documentation\": \" The query failed within the database.\\n https://www.postgresql.org/docs/current/errcodes-appendix.html\"
                    },
                    {
                        \"kind\": \"type-constructor\",
                        \"name\": \"UnexpectedArgumentCount\",
                        \"parameters\": [
                            {
                                \"kind\": \"parameter\",
                                \"type\": {
                                    \"ref\": null,
                                    \"kind\": \"named\",
                                    \"name\": \"Int\",
                                    \"module\": \"gleam\",
                                    \"package\": \"\",
                                    \"parameters\": []
                                },
                                \"label\": \"expected\"
                            },
                            {
                                \"kind\": \"parameter\",
                                \"type\": {
                                    \"ref\": null,
                                    \"kind\": \"named\",
                                    \"name\": \"Int\",
                                    \"module\": \"gleam\",
                                    \"package\": \"\",
                                    \"parameters\": []
                                },
                                \"label\": \"got\"
                            }
                        ],
                        \"documentation\": null
                    },
                    {
                        \"kind\": \"type-constructor\",
                        \"name\": \"UnexpectedArgumentType\",
                        \"parameters\": [
                            {
                                \"kind\": \"parameter\",
                                \"type\": {
                                    \"ref\": null,
                                    \"kind\": \"named\",
                                    \"name\": \"String\",
                                    \"module\": \"gleam\",
                                    \"package\": \"\",
                                    \"parameters\": []
                                },
                                \"label\": \"expected\"
                            },
                            {
                                \"kind\": \"parameter\",
                                \"type\": {
                                    \"ref\": null,
                                    \"kind\": \"named\",
                                    \"name\": \"String\",
                                    \"module\": \"gleam\",
                                    \"package\": \"\",
                                    \"parameters\": []
                                },
                                \"label\": \"got\"
                            }
                        ],
                        \"documentation\": \" One of the arguments supplied was not of the type that the query required.\"
                    },
                    {
                        \"kind\": \"type-constructor\",
                        \"name\": \"UnexpectedResultType\",
                        \"parameters\": [
                            {
                                \"kind\": \"parameter\",
                                \"type\": {
                                    \"ref\": null,
                                    \"kind\": \"named\",
                                    \"name\": \"List\",
                                    \"module\": \"gleam\",
                                    \"package\": \"\",
                                    \"parameters\": [
                                        {
                                            \"ref\": \"0.37.0\",
                                            \"kind\": \"named\",
                                            \"name\": \"DecodeError\",
                                            \"module\": \"gleam/dynamic\",
                                            \"package\": \"gleam_stdlib\",
                                            \"parameters\": []
                                        }
                                    ]
                                },
                                \"label\": null
                            }
                        ],
                        \"documentation\": \" The rows returned by the database could not be decoded using the supplied\\n dynamic decoder.\"
                    },
                    {
                        \"kind\": \"type-constructor\",
                        \"name\": \"ConnectionUnavailable\",
                        \"parameters\": [],
                        \"documentation\": \" No connection was available to execute the query. This may be due to\\n invalid connection details such as an invalid username or password.\"
                    }
                ],
                \"documentation\": null
            },
            \"module_name\": \"gleam/pgo\",
            \"package_name\": \"gleam_pgo\",
            \"version\": \"0.7.0\"
        },
        {
            \"name\": \"refresh\",
            \"documentation\": \"Build a token request using a \\n [Refresh token](https://datatracker.ietf.org/doc/html/rfc6749#section-6)\",
            \"kind\": \"function\",
            \"metadata\": {
                \"deprecation\": null,
                \"implementations\": {
                    \"gleam\": false,
                    \"uses_erlang_externals\": true,
                    \"uses_javascript_externals\": true
                }
            },
            \"json_signature\": {
                \"kind\": \"function\",
                \"name\": \"refresh\",
                \"return\": {
                    \"ref\": \"3.6.0\",
                    \"kind\": \"named\",
                    \"name\": \"Request\",
                    \"module\": \"gleam/http/request\",
                    \"package\": \"gleam_http\",
                    \"parameters\": [
                        {
                            \"ref\": null,
                            \"kind\": \"named\",
                            \"name\": \"String\",
                            \"module\": \"gleam\",
                            \"package\": \"\",
                            \"parameters\": []
                        }
                    ]
                },
                \"parameters\": [
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": \"0.4.0\",
                            \"kind\": \"named\",
                            \"name\": \"Client\",
                            \"module\": \"glow_auth\",
                            \"package\": \"glow_auth\",
                            \"parameters\": [
                                {
                                    \"id\": 0,
                                    \"kind\": \"variable\"
                                }
                            ]
                        },
                        \"label\": null
                    },
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": \"0.4.0\",
                            \"kind\": \"named\",
                            \"name\": \"UriAppendage\",
                            \"module\": \"glow_auth/uri/uri_builder\",
                            \"package\": \"glow_auth\",
                            \"parameters\": []
                        },
                        \"label\": null
                    },
                    {
                        \"kind\": \"parameter\",
                        \"type\": {
                            \"ref\": null,
                            \"kind\": \"named\",
                            \"name\": \"String\",
                            \"module\": \"gleam\",
                            \"package\": \"\",
                            \"parameters\": []
                        },
                        \"label\": null
                    }
                ],
                \"deprecation\": \" Build a token request using a \\n [Refresh token](https://datatracker.ietf.org/doc/html/rfc6749#section-6)\",
                \"documentation\": \" Build a token request using a \\n [Refresh token](https://datatracker.ietf.org/doc/html/rfc6749#section-6)\",
                \"implementations\": {
                    \"gleam\": false,
                    \"uses_erlang_externals\": true,
                    \"uses_javascript_externals\": true
                }
            },
            \"module_name\": \"glow_auth/token_request\",
            \"package_name\": \"glow_auth\",
            \"version\": \"0.4.0\"
        },
        {
            \"name\": \"UriAppendage\",
            \"documentation\": \"Uri Appendage defines a few handy ways of appending to an existing Uri.\\n\\n See `append` for how you can use this.\",
            \"kind\": \"type_definition\",
            \"metadata\": {
                \"deprecation\": null
            },
            \"json_signature\": {
                \"kind\": \"type-definition\",
                \"name\": \"UriAppendage\",
                \"parameters\": 0,
                \"deprecation\": \" Uri Appendage defines a few handy ways of appending to an existing Uri.\\n\\n See `append` for how you can use this.\",
                \"constructors\": [
                    {
                        \"kind\": \"type-constructor\",
                        \"name\": \"RelativePath\",
                        \"parameters\": [
                            {
                                \"kind\": \"parameter\",
                                \"type\": {
                                    \"ref\": null,
                                    \"kind\": \"named\",
                                    \"name\": \"String\",
                                    \"module\": \"gleam\",
                                    \"package\": \"\",
                                    \"parameters\": []
                                },
                                \"label\": null
                            }
                        ],
                        \"documentation\": \" Represent a relative path, expected to be directly appended to existing Uri.\"
                    },
                    {
                        \"kind\": \"type-constructor\",
                        \"name\": \"FullPath\",
                        \"parameters\": [
                            {
                                \"kind\": \"parameter\",
                                \"type\": {
                                    \"ref\": null,
                                    \"kind\": \"named\",
                                    \"name\": \"String\",
                                    \"module\": \"gleam\",
                                    \"package\": \"\",
                                    \"parameters\": []
                                },
                                \"label\": null
                            }
                        ],
                        \"documentation\": \" Represent a full path, expected to completely replace an existing Uri.\"
                    },
                    {
                        \"kind\": \"type-constructor\",
                        \"name\": \"FullUri\",
                        \"parameters\": [
                            {
                                \"kind\": \"parameter\",
                                \"type\": {
                                    \"ref\": \"0.37.0\",
                                    \"kind\": \"named\",
                                    \"name\": \"Uri\",
                                    \"module\": \"gleam/uri\",
                                    \"package\": \"gleam_stdlib\",
                                    \"parameters\": []
                                },
                                \"label\": null
                            }
                        ],
                        \"documentation\": \" Represent a full uri, expected to completely replace the Uri.\"
                    }
                ],
                \"documentation\": \" Uri Appendage defines a few handy ways of appending to an existing Uri.\\n\\n See `append` for how you can use this.\"
            },
            \"module_name\": \"glow_auth/uri/uri_builder\",
            \"package_name\": \"glow_auth\",
            \"version\": \"0.4.0\"
        }
    ]
}"
  |> json.decode(search_result.decode_search_results)
}
