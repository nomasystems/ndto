searchNodes=[{"doc":"","ref":"ndto.html","title":"ndto","type":"module"},{"doc":"Generates an Erlang Syntax Tree of a DTO module from a schema","ref":"ndto.html#generate/2","title":"ndto.generate/2","type":"function"},{"doc":"Equivalent to load(DTO, []) .","ref":"ndto.html#load/1","title":"ndto.load/1","type":"function"},{"doc":"Loads a DTO module into the Erlang Runtime System","ref":"ndto.html#load/2","title":"ndto.load/2","type":"function"},{"doc":"Writes a DTO module to a file","ref":"ndto.html#write/2","title":"ndto.write/2","type":"function"},{"doc":"","ref":"ndto.html#t:array/0","title":"ndto.array/0","type":"type"},{"doc":"","ref":"ndto.html#t:array_schema/0","title":"ndto.array_schema/0","type":"type"},{"doc":"","ref":"ndto.html#t:boolean_schema/0","title":"ndto.boolean_schema/0","type":"type"},{"doc":"","ref":"ndto.html#t:complement_schema/0","title":"ndto.complement_schema/0","type":"type"},{"doc":"","ref":"ndto.html#t:empty_schema/0","title":"ndto.empty_schema/0","type":"type"},{"doc":"","ref":"ndto.html#t:enum_schema/0","title":"ndto.enum_schema/0","type":"type"},{"doc":"","ref":"ndto.html#t:format/0","title":"ndto.format/0","type":"type"},{"doc":"","ref":"ndto.html#t:integer_schema/0","title":"ndto.integer_schema/0","type":"type"},{"doc":"","ref":"ndto.html#t:intersection_schema/0","title":"ndto.intersection_schema/0","type":"type"},{"doc":"","ref":"ndto.html#t:number_schema/0","title":"ndto.number_schema/0","type":"type"},{"doc":"","ref":"ndto.html#t:object/0","title":"ndto.object/0","type":"type"},{"doc":"","ref":"ndto.html#t:object_schema/0","title":"ndto.object_schema/0","type":"type"},{"doc":"","ref":"ndto.html#t:pattern/0","title":"ndto.pattern/0","type":"type"},{"doc":"","ref":"ndto.html#t:ref_schema/0","title":"ndto.ref_schema/0","type":"type"},{"doc":"","ref":"ndto.html#t:schema/0","title":"ndto.schema/0","type":"type"},{"doc":"","ref":"ndto.html#t:string_schema/0","title":"ndto.string_schema/0","type":"type"},{"doc":"","ref":"ndto.html#t:symmetric_difference_schema/0","title":"ndto.symmetric_difference_schema/0","type":"type"},{"doc":"","ref":"ndto.html#t:t/0","title":"ndto.t/0","type":"opaque"},{"doc":"","ref":"ndto.html#t:union_schema/0","title":"ndto.union_schema/0","type":"type"},{"doc":"","ref":"ndto.html#t:universal_schema/0","title":"ndto.universal_schema/0","type":"type"},{"doc":"","ref":"ndto.html#t:value/0","title":"ndto.value/0","type":"type"},{"doc":"ndto is an Erlang library for generating DTO (Data Transfer Object) validation modules from schemas.","ref":"readme.html","title":"Overview","type":"extras"},{"doc":"To use ndto in your project, just add it as a dependency in your rebar.config file: { deps , [ { ndto , { git , &quot;https://github.com/nomasystems/ndto.git&quot; , { branch , &quot;main&quot; } } } ] } .","ref":"readme.html#setup","title":"Overview - Setup","type":"extras"},{"doc":"Define a ndto schema. Schema = \#{ &lt;&lt; &quot;type&quot; &gt;&gt; =&gt; &lt;&lt; &quot;string&quot; &gt;&gt; , &lt;&lt; &quot;minLength&quot; &gt;&gt; =&gt; 8 , &lt;&lt; &quot;pattern&quot; &gt;&gt; =&gt; &lt;&lt; &quot;^hello&quot; &gt;&gt; } . Generate a module using the ndto:generate/2 function. DTO = ndto : generate ( string_schema , Schema ) . Load the generated module on the fly. ok = ndto : load ( DTO ) . Call the is_valid/1 function from the generated module to validate your data. true = string_schema : is_valid ( &lt;&lt; &quot;hello world&quot; &gt;&gt; ) . false = string_schema : is_valid ( &lt;&lt; &quot;hello&quot; &gt;&gt; ) . false = string_schema : is_valid ( &lt;&lt; &quot;hi world&quot; &gt;&gt; ) .","ref":"readme.html#quickstart","title":"Overview - Quickstart","type":"extras"},{"doc":"ndto schemas are defined as follows: - type schema ( ) : : undefined | empty_schema ( ) | universal_schema ( ) | ref_schema ( ) | boolean_schema ( ) | enum_schema ( ) | integer_schema ( ) | number_schema ( ) | string_schema ( ) | array_schema ( ) | object_schema ( ) | union_schema ( ) | intersection_schema ( ) | complement_schema ( ) | symmetric_difference_schema ( ) . - type empty_schema ( ) : : false . - type universal_schema ( ) : : true | \#{ } | union_schema ( ) . - type ref_schema ( ) : : \#{ &lt;&lt; &quot;$ref&quot; &gt;&gt; := binary ( ) } . - type boolean_schema ( ) : : \#{ &lt;&lt; &quot;type&quot; &gt;&gt; := &lt;&lt; &quot;boolean&quot; &gt;&gt; } . - type enum_schema ( ) : : \#{ &lt;&lt; &quot;enum&quot; &gt;&gt; := [ value ( ) ] } . - type integer_schema ( ) : : \#{ &lt;&lt; &quot;type&quot; &gt;&gt; := &lt;&lt; &quot;integer&quot; &gt;&gt; , &lt;&lt; &quot;minimum&quot; &gt;&gt; =&gt; integer ( ) , &lt;&lt; &quot;exclusiveMinimum&quot; &gt;&gt; =&gt; boolean ( ) , &lt;&lt; &quot;maximum&quot; &gt;&gt; =&gt; integer ( ) , &lt;&lt; &quot;exclusiveMaximum&quot; &gt;&gt; =&gt; boolean ( ) , &lt;&lt; &quot;multipleOf&quot; &gt;&gt; =&gt; integer ( ) } . - type number_schema ( ) : : \#{ &lt;&lt; &quot;type&quot; &gt;&gt; := &lt;&lt; &quot;number&quot; &gt;&gt; , &lt;&lt; &quot;minimum&quot; &gt;&gt; =&gt; number ( ) , &lt;&lt; &quot;exclusiveMinimum&quot; &gt;&gt; =&gt; boolean ( ) , &lt;&lt; &quot;maximum&quot; &gt;&gt; =&gt; number ( ) , &lt;&lt; &quot;exclusiveMaximum&quot; &gt;&gt; =&gt; boolean ( ) } . - type string_schema ( ) : : \#{ &lt;&lt; &quot;type&quot; &gt;&gt; := &lt;&lt; &quot;string&quot; &gt;&gt; , &lt;&lt; &quot;minimum&quot; &gt;&gt; =&gt; integer ( ) , &lt;&lt; &quot;minLength&quot; &gt;&gt; =&gt; non_neg_integer ( ) , &lt;&lt; &quot;maxLength&quot; &gt;&gt; =&gt; non_neg_integer ( ) , &lt;&lt; &quot;format&quot; &gt;&gt; =&gt; format ( ) , &lt;&lt; &quot;pattern&quot; &gt;&gt; =&gt; pattern ( ) } . - type array_schema ( ) : : \#{ &lt;&lt; &quot;type&quot; &gt;&gt; := &lt;&lt; &quot;array&quot; &gt;&gt; , &lt;&lt; &quot;items&quot; &gt;&gt; =&gt; schema ( ) , &lt;&lt; &quot;minItems&quot; &gt;&gt; =&gt; non_neg_integer ( ) , &lt;&lt; &quot;maxItems&quot; &gt;&gt; =&gt; non_neg_integer ( ) , &lt;&lt; &quot;uniqueItems&quot; &gt;&gt; =&gt; boolean ( ) } . - type object_schema ( ) : : \#{ &lt;&lt; &quot;type&quot; &gt;&gt; := &lt;&lt; &quot;object&quot; &gt;&gt; , &lt;&lt; &quot;properties&quot; &gt;&gt; =&gt; \#{ binary ( ) =&gt; schema ( ) } , &lt;&lt; &quot;required&quot; &gt;&gt; =&gt; [ binary ( ) ] , &lt;&lt; &quot;minProperties&quot; &gt;&gt; =&gt; non_neg_integer ( ) , &lt;&lt; &quot;maxProperties&quot; &gt;&gt; =&gt; non_neg_integer ( ) , &lt;&lt; &quot;patternProperties&quot; &gt;&gt; =&gt; \#{ pattern ( ) =&gt; schema ( ) } , &lt;&lt; &quot;additionalProperties&quot; &gt;&gt; =&gt; schema ( ) } . - type union_schema ( ) : : \#{ &lt;&lt; &quot;anyOf&quot; &gt;&gt; := [ schema ( ) ] } . - type intersection_schema ( ) : : \#{ &lt;&lt; &quot;allOf&quot; &gt;&gt; := [ schema ( ) ] } . - type complement_schema ( ) : : \#{ &lt;&lt; &quot;not&quot; &gt;&gt; := schema ( ) } . - type symmetric_difference_schema ( ) : : \#{ &lt;&lt; &quot;oneOf&quot; &gt;&gt; := [ schema ( ) ] } . - type value ( ) : : boolean ( ) | integer ( ) | float ( ) | binary ( ) | array ( ) | object ( ) . - type array ( ) : : [ value ( ) ] . - type object ( ) : : \#{ binary ( ) =&gt; value ( ) } . - type format ( ) : : &lt;&lt; &quot;iso8601&quot; &gt;&gt; | &lt;&lt; &quot;base64&quot; &gt;&gt; . - type pattern ( ) : : binary ( ) .","ref":"readme.html#schema","title":"Overview - Schema","type":"extras"},{"doc":"We :heart: contributions! Please feel free to submit issues, create pull requests or just spread the word about ndto in the open-source community. Together, we can make this library even better! :muscle:","ref":"readme.html#contributing","title":"Overview - Contributing","type":"extras"},{"doc":"If you need help or have any questions, please don't hesitate to open an issue on the GitHub repository or contact the maintainers directly.","ref":"readme.html#support","title":"Overview - Support","type":"extras"},{"doc":"ndto is released under the Apache 2.0 License. For more information, please see the LICENSE file.","ref":"readme.html#license","title":"Overview - License","type":"extras"},{"doc":"Apache License Version 2.0, January 2004 http://www.apache.org/licenses/ TERMS AND CONDITIONS FOR USE, REPRODUCTION, AND DISTRIBUTION 1. Definitions. &quot;License&quot; shall mean the terms and conditions for use, reproduction, and distribution as defined by Sections 1 through 9 of this document. &quot;Licensor&quot; shall mean the copyright owner or entity authorized by the copyright owner that is granting the License. &quot;Legal Entity&quot; shall mean the union of the acting entity and all other entities that control, are controlled by, or are under common control with that entity. For the purposes of this definition, &quot;control&quot; means (i) the power, direct or indirect, to cause the direction or management of such entity, whether by contract or otherwise, or (ii) ownership of fifty percent (50%) or more of the outstanding shares, or (iii) beneficial ownership of such entity. &quot;You&quot; (or &quot;Your&quot;) shall mean an individual or Legal Entity exercising permissions granted by this License. &quot;Source&quot; form shall mean the preferred form for making modifications, including but not limited to software source code, documentation source, and configuration files. &quot;Object&quot; form shall mean any form resulting from mechanical transformation or translation of a Source form, including but not limited to compiled object code, generated documentation, and conversions to other media types. &quot;Work&quot; shall mean the work of authorship, whether in Source or Object form, made available under the License, as indicated by a copyright notice that is included in or attached to the work (an example is provided in the Appendix below). &quot;Derivative Works&quot; shall mean any work, whether in Source or Object form, that is based on (or derived from) the Work and for which the editorial revisions, annotations, elaborations, or other modifications represent, as a whole, an original work of authorship. For the purposes of this License, Derivative Works shall not include works that remain separable from, or merely link (or bind by name) to the interfaces of, the Work and Derivative Works thereof. &quot;Contribution&quot; shall mean any work of authorship, including the original version of the Work and any modifications or additions to that Work or Derivative Works thereof, that is intentionally submitted to Licensor for inclusion in the Work by the copyright owner or by an individual or Legal Entity authorized to submit on behalf of the copyright owner. For the purposes of this definition, &quot;submitted&quot; means any form of electronic, verbal, or written communication sent to the Licensor or its representatives, including but not limited to communication on electronic mailing lists, source code control systems, and issue tracking systems that are managed by, or on behalf of, the Licensor for the purpose of discussing and improving the Work, but excluding communication that is conspicuously marked or otherwise designated in writing by the copyright owner as &quot;Not a Contribution.&quot; &quot;Contributor&quot; shall mean Licensor and any individual or Legal Entity on behalf of whom a Contribution has been received by Licensor and subsequently incorporated within the Work. 2. Grant of Copyright License. Subject to the terms and conditions of this License, each Contributor hereby grants to You a perpetual, worldwide, non-exclusive, no-charge, royalty-free, irrevocable copyright license to reproduce, prepare Derivative Works of, publicly display, publicly perform, sublicense, and distribute the Work and such Derivative Works in Source or Object form. 3. Grant of Patent License. Subject to the terms and conditions of this License, each Contributor hereby grants to You a perpetual, worldwide, non-exclusive, no-charge, royalty-free, irrevocable (except as stated in this section) patent license to make, have made, use, offer to sell, sell, import, and otherwise transfer the Work, where such license applies only to those patent claims licensable by such Contributor that are necessarily infringed by their Contribution(s) alone or by combination of their Contribution(s) with the Work to which such Contribution(s) was submitted. If You institute patent litigation against any entity (including a cross-claim or counterclaim in a lawsuit) alleging that the Work or a Contribution incorporated within the Work constitutes direct or contributory patent infringement, then any patent licenses granted to You under this License for that Work shall terminate as of the date such litigation is filed. 4. Redistribution. You may reproduce and distribute copies of the Work or Derivative Works thereof in any medium, with or without modifications, and in Source or Object form, provided that You meet the following conditions: (a) You must give any other recipients of the Work or Derivative Works a copy of this License; and (b) You must cause any modified files to carry prominent notices stating that You changed the files; and (c) You must retain, in the Source form of any Derivative Works that You distribute, all copyright, patent, trademark, and attribution notices from the Source form of the Work, excluding those notices that do not pertain to any part of the Derivative Works; and (d) If the Work includes a &quot;NOTICE&quot; text file as part of its distribution, then any Derivative Works that You distribute must include a readable copy of the attribution notices contained within such NOTICE file, excluding those notices that do not pertain to any part of the Derivative Works, in at least one of the following places: within a NOTICE text file distributed as part of the Derivative Works; within the Source form or documentation, if provided along with the Derivative Works; or, within a display generated by the Derivative Works, if and wherever such third-party notices normally appear. The contents of the NOTICE file are for informational purposes only and do not modify the License. You may add Your own attribution notices within Derivative Works that You distribute, alongside or as an addendum to the NOTICE text from the Work, provided that such additional attribution notices cannot be construed as modifying the License. You may add Your own copyright statement to Your modifications and may provide additional or different license terms and conditions for use, reproduction, or distribution of Your modifications, or for any such Derivative Works as a whole, provided Your use, reproduction, and distribution of the Work otherwise complies with the conditions stated in this License. 5. Submission of Contributions. Unless You explicitly state otherwise, any Contribution intentionally submitted for inclusion in the Work by You to the Licensor shall be under the terms and conditions of this License, without any additional terms or conditions. Notwithstanding the above, nothing herein shall supersede or modify the terms of any separate license agreement you may have executed with Licensor regarding such Contributions. 6. Trademarks. This License does not grant permission to use the trade names, trademarks, service marks, or product names of the Licensor, except as required for reasonable and customary use in describing the origin of the Work and reproducing the content of the NOTICE file. 7. Disclaimer of Warranty. Unless required by applicable law or agreed to in writing, Licensor provides the Work (and each Contributor provides its Contributions) on an &quot;AS IS&quot; BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied, including, without limitation, any warranties or conditions of TITLE, NON-INFRINGEMENT, MERCHANTABILITY, or FITNESS FOR A PARTICULAR PURPOSE. You are solely responsible for determining the appropriateness of using or redistributing the Work and assume any risks associated with Your exercise of permissions under this License. 8. Limitation of Liability. In no event and under no legal theory, whether in tort (including negligence), contract, or otherwise, unless required by applicable law (such as deliberate and grossly negligent acts) or agreed to in writing, shall any Contributor be liable to You for damages, including any direct, indirect, special, incidental, or consequential damages of any character arising as a result of this License or out of the use or inability to use the Work (including but not limited to damages for loss of goodwill, work stoppage, computer failure or malfunction, or any and all other commercial damages or losses), even if such Contributor has been advised of the possibility of such damages. 9. Accepting Warranty or Additional Liability. While redistributing the Work or Derivative Works thereof, You may choose to offer, and charge a fee for, acceptance of support, warranty, indemnity, or other liability obligations and/or rights consistent with this License. However, in accepting such obligations, You may act only on Your own behalf and on Your sole responsibility, not on behalf of any other Contributor, and only if You agree to indemnify, defend, and hold each Contributor harmless for any liability incurred by, or claims asserted against, such Contributor by reason of your accepting any such warranty or additional liability. END OF TERMS AND CONDITIONS APPENDIX: How to apply the Apache License to your work. To apply the Apache License to your work, attach the following boilerplate notice, with the fields enclosed by brackets &quot;[]&quot; replaced with your own identifying information. (Don't include the brackets!) The text should be enclosed in the appropriate comment syntax for the file format. We also recommend that a file or class name and description of purpose be included on the same &quot;printed page&quot; as the copyright notice for easier identification within third-party archives. Copyright [yyyy] [name of copyright owner] Licensed under the Apache License, Version 2.0 (the &quot;License&quot;); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0 Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an &quot;AS IS&quot; BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.","ref":"license.html","title":"License","type":"extras"}]