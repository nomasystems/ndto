searchData={"content_type":"text/plain","items":[{"doc":"ndto 's main module.","ref":"ndto.html","title":"ndto","type":"module"},{"doc":"Generates an Erlang Syntax Tree of a DTO module from a schema","ref":"ndto.html#generate/2","title":"ndto.generate/2","type":"function"},{"doc":"Equivalent to load(DTO, []) .","ref":"ndto.html#load/1","title":"ndto.load/1","type":"function"},{"doc":"Loads a DTO module into the Erlang Runtime System","ref":"ndto.html#load/2","title":"ndto.load/2","type":"function"},{"doc":"Writes a DTO module to a file","ref":"ndto.html#write/2","title":"ndto.write/2","type":"function"},{"doc":null,"ref":"ndto.html#t:array/0","title":"ndto.array/0","type":"type"},{"doc":"#{ &lt;&lt;&quot;type&quot;&gt;&gt; := &lt;&lt;&quot;array&quot;&gt;&gt;, &lt;&lt;&quot;items&quot;&gt;&gt; =&gt; schema(), &lt;&lt;&quot;minItems&quot;&gt;&gt; =&gt; non_neg_integer(), &lt;&lt;&quot;maxItems&quot;&gt;&gt; =&gt; non_neg_integer(), &lt;&lt;&quot;uniqueItems&quot;&gt;&gt; =&gt; boolean() }","ref":"ndto.html#t:array_schema/0","title":"ndto.array_schema/0","type":"type"},{"doc":"#{&lt;&lt;&quot;type&quot;&gt;&gt; := &lt;&lt;&quot;boolean&quot;&gt;&gt;}","ref":"ndto.html#t:boolean_schema/0","title":"ndto.boolean_schema/0","type":"type"},{"doc":"#{&lt;&lt;&quot;not&quot;&gt;&gt; := schema()}","ref":"ndto.html#t:complement_schema/0","title":"ndto.complement_schema/0","type":"type"},{"doc":null,"ref":"ndto.html#t:empty_schema/0","title":"ndto.empty_schema/0","type":"type"},{"doc":"#{&lt;&lt;&quot;enum&quot;&gt;&gt; := [value()]}","ref":"ndto.html#t:enum_schema/0","title":"ndto.enum_schema/0","type":"type"},{"doc":"&lt;&lt;&quot;iso8601&quot;&gt;&gt; | &lt;&lt;&quot;base64&quot;&gt;&gt;","ref":"ndto.html#t:format/0","title":"ndto.format/0","type":"type"},{"doc":"#{ &lt;&lt;&quot;type&quot;&gt;&gt; := &lt;&lt;&quot;integer&quot;&gt;&gt;, &lt;&lt;&quot;minimum&quot;&gt;&gt; =&gt; integer(), &lt;&lt;&quot;exclusiveMinimum&quot;&gt;&gt; =&gt; boolean(), &lt;&lt;&quot;maximum&quot;&gt;&gt; =&gt; integer(), &lt;&lt;&quot;exclusiveMaximum&quot;&gt;&gt; =&gt; boolean(), &lt;&lt;&quot;multipleOf&quot;&gt;&gt; =&gt; integer() }","ref":"ndto.html#t:integer_schema/0","title":"ndto.integer_schema/0","type":"type"},{"doc":"#{&lt;&lt;&quot;allOf&quot;&gt;&gt; := [schema()]}","ref":"ndto.html#t:intersection_schema/0","title":"ndto.intersection_schema/0","type":"type"},{"doc":null,"ref":"ndto.html#t:name/0","title":"ndto.name/0","type":"type"},{"doc":"#{ &lt;&lt;&quot;type&quot;&gt;&gt; := &lt;&lt;&quot;number&quot;&gt;&gt;, &lt;&lt;&quot;minimum&quot;&gt;&gt; =&gt; integer(), &lt;&lt;&quot;exclusiveMinimum&quot;&gt;&gt; =&gt; boolean(), &lt;&lt;&quot;maximum&quot;&gt;&gt; =&gt; integer(), &lt;&lt;&quot;exclusiveMaximum&quot;&gt;&gt; =&gt; boolean(), &lt;&lt;&quot;multipleOf&quot;&gt;&gt; =&gt; integer() }","ref":"ndto.html#t:number_schema/0","title":"ndto.number_schema/0","type":"type"},{"doc":null,"ref":"ndto.html#t:object/0","title":"ndto.object/0","type":"type"},{"doc":"#{ &lt;&lt;&quot;type&quot;&gt;&gt; := &lt;&lt;&quot;object&quot;&gt;&gt;, &lt;&lt;&quot;properties&quot;&gt;&gt; =&gt; #{binary() =&gt; schema()}, &lt;&lt;&quot;required&quot;&gt;&gt; =&gt; [binary()], &lt;&lt;&quot;minProperties&quot;&gt;&gt; =&gt; non_neg_integer(), &lt;&lt;&quot;maxProperties&quot;&gt;&gt; =&gt; non_neg_integer(), &lt;&lt;&quot;patternProperties&quot;&gt;&gt; =&gt; #{pattern() =&gt; schema()}, &lt;&lt;&quot;additionalProperties&quot;&gt;&gt; =&gt; schema() }","ref":"ndto.html#t:object_schema/0","title":"ndto.object_schema/0","type":"type"},{"doc":null,"ref":"ndto.html#t:pattern/0","title":"ndto.pattern/0","type":"type"},{"doc":"#{&lt;&lt;&quot;ref&quot;&gt;&gt; := binary()}","ref":"ndto.html#t:ref_schema/0","title":"ndto.ref_schema/0","type":"type"},{"doc":null,"ref":"ndto.html#t:schema/0","title":"ndto.schema/0","type":"type"},{"doc":"#{ &lt;&lt;&quot;type&quot;&gt;&gt; := &lt;&lt;&quot;string&quot;&gt;&gt;, &lt;&lt;&quot;minLength&quot;&gt;&gt; =&gt; non_neg_integer(), &lt;&lt;&quot;maxLength&quot;&gt;&gt; =&gt; non_neg_integer(), &lt;&lt;&quot;format&quot;&gt;&gt; =&gt; format(), &lt;&lt;&quot;pattern&quot;&gt;&gt; =&gt; pattern() }","ref":"ndto.html#t:string_schema/0","title":"ndto.string_schema/0","type":"type"},{"doc":"#{&lt;&lt;&quot;oneOf&quot;&gt;&gt; := [schema()]}","ref":"ndto.html#t:symmetric_difference_schema/0","title":"ndto.symmetric_difference_schema/0","type":"type"},{"doc":null,"ref":"ndto.html#t:t/0","title":"ndto.t/0","type":"type"},{"doc":"#{&lt;&lt;&quot;anyOf&quot;&gt;&gt; := [schema()]}","ref":"ndto.html#t:union_schema/0","title":"ndto.union_schema/0","type":"type"},{"doc":null,"ref":"ndto.html#t:universal_schema/0","title":"ndto.universal_schema/0","type":"type"},{"doc":null,"ref":"ndto.html#t:value/0","title":"ndto.value/0","type":"type"},{"doc":"An ndto behaviour for schema parsers.","ref":"ndto_parser.html","title":"ndto_parser","type":"behaviour"},{"doc":"Parses a specification into a ndto:schema()","ref":"ndto_parser.html#c:parse/2","title":"ndto_parser.parse/2","type":"callback"},{"doc":"Parses a schema specification into a ndto:schema() using the given parser.","ref":"ndto_parser.html#parse/3","title":"ndto_parser.parse/3","type":"function"},{"doc":"A parser is a module that implements the ndto_parser behaviour.","ref":"ndto_parser.html#t:t/0","title":"ndto_parser.t/0","type":"opaque"},{"doc":"An ndto_parser for draft-04 JSON Schema specifications.","ref":"ndto_parser_json_schema_draft_04.html","title":"ndto_parser_json_schema_draft_04","type":"module"},{"doc":"Parses a draft-04 JSON Schema specification into a list of ndto:schema() values.","ref":"ndto_parser_json_schema_draft_04.html#parse/2","title":"ndto_parser_json_schema_draft_04.parse/2","type":"function"},{"doc":null,"ref":"ndto_parser_json_schema_draft_04.html#t:json_schema/0","title":"ndto_parser_json_schema_draft_04.json_schema/0","type":"type"},{"doc":null,"ref":"ndto_schema.html","title":"ndto_schema","type":"module"},{"doc":null,"ref":"ndto_schema.html#complement/1","title":"ndto_schema.complement/1","type":"function"},{"doc":null,"ref":"ndto_schema.html#empty_schema/0","title":"ndto_schema.empty_schema/0","type":"function"},{"doc":null,"ref":"ndto_schema.html#intersection/1","title":"ndto_schema.intersection/1","type":"function"},{"doc":null,"ref":"ndto_schema.html#multiples/3","title":"ndto_schema.multiples/3","type":"function"},{"doc":null,"ref":"ndto_schema.html#symmetric_difference/1","title":"ndto_schema.symmetric_difference/1","type":"function"},{"doc":null,"ref":"ndto_schema.html#union/1","title":"ndto_schema.union/1","type":"function"},{"doc":null,"ref":"ndto_schema.html#universal_schema/0","title":"ndto_schema.universal_schema/0","type":"function"},{"doc":"# ndto\n[![ndto ci](https://github.com/nomasystems/ndto/actions/workflows/ci.yml/badge.svg)](https://github.com/nomasystems/ndto/actions/workflows/ci.yml)\n[![ndto docs](https://github.com/nomasystems/ndto/actions/workflows/docs.yml/badge.svg)](https://nomasystems.github.io/ndto)\n\n`ndto` is an Erlang library for generating DTO (Data Transfer Object) validation modules from schemas.","ref":"readme.html","title":"Overview","type":"extras"},{"doc":"To use `ndto` in your project, just add it as a dependency in your `rebar.config` file:\n\n```erl\n{deps, [\n    {ndto, {git, \"https://github.com/nomasystems/ndto.git\", {branch, \"main\"}}}\n]}.\n```","ref":"readme.html#setup","title":"Setup - Overview","type":"extras"},{"doc":"1. Define an `ndto` schema.\n```erl\nSchema = #{\n     > =>  >,\n     > => 8,\n     > =>  >\n}.\n```\n\n2. Generate a module using the `ndto:generate/2` function.\n```erl\nDTO = ndto:generate(string_schema, Schema).\n```\n\n3. Load the generated module on the fly.\n```erl\nok = ndto:load(DTO).\n```\n\n4. Call the `is_valid/1` function from the generated module to validate your data.\n```erl\ntrue = string_schema:is_valid( >).\nfalse = string_schema:is_valid( >).\nfalse = string_schema:is_valid( >).\n```","ref":"readme.html#quickstart","title":"Quickstart - Overview","type":"extras"},{"doc":"We :heart: contributions! Please feel free to submit issues, create pull requests or just spread the word about `ndto` in the open-source community. Don't forget to check out our [contribution guidelines](CONTRIBUTING.md) to ensure smooth collaboration! :rocket:","ref":"readme.html#contributing","title":"Contributing - Overview","type":"extras"},{"doc":"If you need help or have any questions, please don't hesitate to open an issue or contact the maintainers directly.","ref":"readme.html#support","title":"Support - Overview","type":"extras"},{"doc":"`ndto` is released under the Apache 2.0 License. For more information, please see the [LICENSE](LICENSE) file.\n> This project uses OpenAPI specification (OAS) schemas and examples, which are licensed under the Apache 2.0 license. See the associated [LICENSE](priv/oas/LICENSE) file for more information.","ref":"readme.html#license","title":"License - Overview","type":"extras"},{"doc":"Apache License\n                           Version 2.0, January 2004\n                        http://www.apache.org/licenses/\n\n   TERMS AND CONDITIONS FOR USE, REPRODUCTION, AND DISTRIBUTION\n\n   1. Definitions.\n\n      \"License\" shall mean the terms and conditions for use, reproduction,\n      and distribution as defined by Sections 1 through 9 of this document.\n\n      \"Licensor\" shall mean the copyright owner or entity authorized by\n      the copyright owner that is granting the License.\n\n      \"Legal Entity\" shall mean the union of the acting entity and all\n      other entities that control, are controlled by, or are under common\n      control with that entity. For the purposes of this definition,\n      \"control\" means (i) the power, direct or indirect, to cause the\n      direction or management of such entity, whether by contract or\n      otherwise, or (ii) ownership of fifty percent (50%) or more of the\n      outstanding shares, or (iii) beneficial ownership of such entity.\n\n      \"You\" (or \"Your\") shall mean an individual or Legal Entity\n      exercising permissions granted by this License.\n\n      \"Source\" form shall mean the preferred form for making modifications,\n      including but not limited to software source code, documentation\n      source, and configuration files.\n\n      \"Object\" form shall mean any form resulting from mechanical\n      transformation or translation of a Source form, including but\n      not limited to compiled object code, generated documentation,\n      and conversions to other media types.\n\n      \"Work\" shall mean the work of authorship, whether in Source or\n      Object form, made available under the License, as indicated by a\n      copyright notice that is included in or attached to the work\n      (an example is provided in the Appendix below).\n\n      \"Derivative Works\" shall mean any work, whether in Source or Object\n      form, that is based on (or derived from) the Work and for which the\n      editorial revisions, annotations, elaborations, or other modifications\n      represent, as a whole, an original work of authorship. For the purposes\n      of this License, Derivative Works shall not include works that remain\n      separable from, or merely link (or bind by name) to the interfaces of,\n      the Work and Derivative Works thereof.\n\n      \"Contribution\" shall mean any work of authorship, including\n      the original version of the Work and any modifications or additions\n      to that Work or Derivative Works thereof, that is intentionally\n      submitted to Licensor for inclusion in the Work by the copyright owner\n      or by an individual or Legal Entity authorized to submit on behalf of\n      the copyright owner. For the purposes of this definition, \"submitted\"\n      means any form of electronic, verbal, or written communication sent\n      to the Licensor or its representatives, including but not limited to\n      communication on electronic mailing lists, source code control systems,\n      and issue tracking systems that are managed by, or on behalf of, the\n      Licensor for the purpose of discussing and improving the Work, but\n      excluding communication that is conspicuously marked or otherwise\n      designated in writing by the copyright owner as \"Not a Contribution.\"\n\n      \"Contributor\" shall mean Licensor and any individual or Legal Entity\n      on behalf of whom a Contribution has been received by Licensor and\n      subsequently incorporated within the Work.\n\n   2. Grant of Copyright License. Subject to the terms and conditions of\n      this License, each Contributor hereby grants to You a perpetual,\n      worldwide, non-exclusive, no-charge, royalty-free, irrevocable\n      copyright license to reproduce, prepare Derivative Works of,\n      publicly display, publicly perform, sublicense, and distribute the\n      Work and such Derivative Works in Source or Object form.\n\n   3. Grant of Patent License. Subject to the terms and conditions of\n      this License, each Contributor hereby grants to You a perpetual,\n      worldwide, non-exclusive, no-charge, royalty-free, irrevocable\n      (except as stated in this section) patent license to make, have made,\n      use, offer to sell, sell, import, and otherwise transfer the Work,\n      where such license applies only to those patent claims licensable\n      by such Contributor that are necessarily infringed by their\n      Contribution(s) alone or by combination of their Contribution(s)\n      with the Work to which such Contribution(s) was submitted. If You\n      institute patent litigation against any entity (including a\n      cross-claim or counterclaim in a lawsuit) alleging that the Work\n      or a Contribution incorporated within the Work constitutes direct\n      or contributory patent infringement, then any patent licenses\n      granted to You under this License for that Work shall terminate\n      as of the date such litigation is filed.\n\n   4. Redistribution. You may reproduce and distribute copies of the\n      Work or Derivative Works thereof in any medium, with or without\n      modifications, and in Source or Object form, provided that You\n      meet the following conditions:\n\n      (a) You must give any other recipients of the Work or\n          Derivative Works a copy of this License; and\n\n      (b) You must cause any modified files to carry prominent notices\n          stating that You changed the files; and\n\n      (c) You must retain, in the Source form of any Derivative Works\n          that You distribute, all copyright, patent, trademark, and\n          attribution notices from the Source form of the Work,\n          excluding those notices that do not pertain to any part of\n          the Derivative Works; and\n\n      (d) If the Work includes a \"NOTICE\" text file as part of its\n          distribution, then any Derivative Works that You distribute must\n          include a readable copy of the attribution notices contained\n          within such NOTICE file, excluding those notices that do not\n          pertain to any part of the Derivative Works, in at least one\n          of the following places: within a NOTICE text file distributed\n          as part of the Derivative Works; within the Source form or\n          documentation, if provided along with the Derivative Works; or,\n          within a display generated by the Derivative Works, if and\n          wherever such third-party notices normally appear. The contents\n          of the NOTICE file are for informational purposes only and\n          do not modify the License. You may add Your own attribution\n          notices within Derivative Works that You distribute, alongside\n          or as an addendum to the NOTICE text from the Work, provided\n          that such additional attribution notices cannot be construed\n          as modifying the License.\n\n      You may add Your own copyright statement to Your modifications and\n      may provide additional or different license terms and conditions\n      for use, reproduction, or distribution of Your modifications, or\n      for any such Derivative Works as a whole, provided Your use,\n      reproduction, and distribution of the Work otherwise complies with\n      the conditions stated in this License.\n\n   5. Submission of Contributions. Unless You explicitly state otherwise,\n      any Contribution intentionally submitted for inclusion in the Work\n      by You to the Licensor shall be under the terms and conditions of\n      this License, without any additional terms or conditions.\n      Notwithstanding the above, nothing herein shall supersede or modify\n      the terms of any separate license agreement you may have executed\n      with Licensor regarding such Contributions.\n\n   6. Trademarks. This License does not grant permission to use the trade\n      names, trademarks, service marks, or product names of the Licensor,\n      except as required for reasonable and customary use in describing the\n      origin of the Work and reproducing the content of the NOTICE file.\n\n   7. Disclaimer of Warranty. Unless required by applicable law or\n      agreed to in writing, Licensor provides the Work (and each\n      Contributor provides its Contributions) on an \"AS IS\" BASIS,\n      WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or\n      implied, including, without limitation, any warranties or conditions\n      of TITLE, NON-INFRINGEMENT, MERCHANTABILITY, or FITNESS FOR A\n      PARTICULAR PURPOSE. You are solely responsible for determining the\n      appropriateness of using or redistributing the Work and assume any\n      risks associated with Your exercise of permissions under this License.\n\n   8. Limitation of Liability. In no event and under no legal theory,\n      whether in tort (including negligence), contract, or otherwise,\n      unless required by applicable law (such as deliberate and grossly\n      negligent acts) or agreed to in writing, shall any Contributor be\n      liable to You for damages, including any direct, indirect, special,\n      incidental, or consequential damages of any character arising as a\n      result of this License or out of the use or inability to use the\n      Work (including but not limited to damages for loss of goodwill,\n      work stoppage, computer failure or malfunction, or any and all\n      other commercial damages or losses), even if such Contributor\n      has been advised of the possibility of such damages.\n\n   9. Accepting Warranty or Additional Liability. While redistributing\n      the Work or Derivative Works thereof, You may choose to offer,\n      and charge a fee for, acceptance of support, warranty, indemnity,\n      or other liability obligations and/or rights consistent with this\n      License. However, in accepting such obligations, You may act only\n      on Your own behalf and on Your sole responsibility, not on behalf\n      of any other Contributor, and only if You agree to indemnify,\n      defend, and hold each Contributor harmless for any liability\n      incurred by, or claims asserted against, such Contributor by reason\n      of your accepting any such warranty or additional liability.\n\n   END OF TERMS AND CONDITIONS\n\n   APPENDIX: How to apply the Apache License to your work.\n\n      To apply the Apache License to your work, attach the following\n      boilerplate notice, with the fields enclosed by brackets \"[]\"\n      replaced with your own identifying information. (Don't include\n      the brackets!)  The text should be enclosed in the appropriate\n      comment syntax for the file format. We also recommend that a\n      file or class name and description of purpose be included on the\n      same \"printed page\" as the copyright notice for easier\n      identification within third-party archives.\n\n   Copyright [yyyy] [name of copyright owner]\n\n   Licensed under the Apache License, Version 2.0 (the \"License\");\n   you may not use this file except in compliance with the License.\n   You may obtain a copy of the License at\n\n       http://www.apache.org/licenses/LICENSE-2.0\n\n   Unless required by applicable law or agreed to in writing, software\n   distributed under the License is distributed on an \"AS IS\" BASIS,\n   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.\n   See the License for the specific language governing permissions and\n   limitations under the License.","ref":"license.html","title":"License","type":"extras"}]}