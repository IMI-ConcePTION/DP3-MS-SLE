---
weight: 2
name_excel: "D3_output_spells_category.xlsx"
description: "contains the spells exited from CreateSpells, i.e., all the continuous spells of observation period of each person, stratified per op_meaning. op_meaning is by default the same for all observation periods, and is set in 05_subpopulations_restricting_meanings for those data sources where the analysis is conducted on subpopulations having different sets of data banks"
slug: "D3_output_spells_category"
datetime: 1.7194827e+09
title: D3_output_spells_category
author: ''
date: '2024-06-27'
categories: []
tags: []
archetype: codebook
output: html_document
---

<script src="/rmarkdown-libs/core-js/shim.min.js"></script>
<script src="/rmarkdown-libs/react/react.min.js"></script>
<script src="/rmarkdown-libs/react/react-dom.min.js"></script>
<script src="/rmarkdown-libs/reactwidget/react-tools.js"></script>
<script src="/rmarkdown-libs/htmlwidgets/htmlwidgets.js"></script>
<link href="/rmarkdown-libs/reactable/reactable.css" rel="stylesheet" />
<script src="/rmarkdown-libs/reactable-binding/reactable.js"></script>
<div class="tab">
<button class="tablinks" onclick="openCity(event, &#39;Metadata&#39;)" id="defaultOpen">Metadata</button>
<button class="tablinks" onclick="openCity(event, &#39;Data Model&#39;)">Data Model</button>
<button class="tablinks" onclick="openCity(event, &#39;Parameters&#39;)">Parameters</button>
<button class="tablinks" onclick="openCity(event, &#39;Example&#39;)">Example</button>
</div>
<div id="Metadata" class="tabcontent">
<div id="htmlwidget-1" class="reactable html-widget" style="width:auto;height:600px;"></div>
<script type="application/json" data-for="htmlwidget-1">{"x":{"tag":{"name":"Reactable","attribs":{"data":{"medatata_name":["name of the D3","content of the D3","Unit of observation","Dataset where the list of UoOs is fully listed and with 1 record per UoO","How many observations per UoO","NxUoO","Variables capturing the UoO","Primary key","Parameters",null,null,null,null,null,null,null,null,null,null,null],"metadata_content":["D3_output_spells_category","contains the spells exited from CreateSpells, i.e., all the continuous spells of observation period of each person, stratified per op_meaning. op_meaning is by default the same for all observation periods, and is set in 05_subpopulations_restricting_meanings for those data sources where the analysis is conducted on subpopulations having different sets of data banks","a person in OBSERVATION_PERIODS","itself","as many as the spells of that person with that op_meaning",">=1","person_id","person_id op_meaning num_spell",null,null,null,null,null,null,null,null,null,null,null,null]},"columns":[{"id":"medatata_name","name":"medatata_name","type":"character"},{"id":"metadata_content","name":"metadata_content","type":"character"}],"sortable":false,"searchable":true,"pagination":false,"highlight":true,"bordered":true,"striped":true,"style":{"maxWidth":1800},"height":"600px","dataKey":"26b1193c7c5a2a66e1cfb1cbde1db323"},"children":[]},"class":"reactR_markup"},"evals":[],"jsHooks":[]}</script>
</div>
<div id="Data Model" class="tabcontent">
<div id="htmlwidget-2" class="reactable html-widget" style="width:auto;height:600px;"></div>
<script type="application/json" data-for="htmlwidget-2">{"x":{"tag":{"name":"Reactable","attribs":{"data":{"Variable":["person_id","entry_spell_category","exit_spell_category","op_meaning","num_spell",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"Description":["unique person identifier",null,null,"if there are subpopulations, this variable indicates to swhich subpopulation the spelòl belongs","ordinal number of the spell of the person",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"Format":["character","date","date","categorical","binary",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"Vocabulary":[null,"1 = imputed\n0 = otherwise","1 = imputed\n0 = otherwise","depends on subpopulations","1 = imputed\n0 = otherwise",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"Notes and examples":["from CDM PERSONS",null,null,"created by the program based on subpopulations; if there are no subpopulations, this variable is '_overall' for all spells",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"retrieved":["yes",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"computed":[null,"yes","yes","yes","yes",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null]},"columns":[{"id":"Variable","name":"Variable","type":"character"},{"id":"Description","name":"Description","type":"character"},{"id":"Format","name":"Format","type":"character"},{"id":"Vocabulary","name":"Vocabulary","type":"character"},{"id":"Notes and examples","name":"Notes and examples","type":"character"},{"id":"retrieved","name":"retrieved","type":"character"},{"id":"computed","name":"computed","type":"character"}],"sortable":false,"searchable":true,"pagination":false,"highlight":true,"bordered":true,"striped":true,"style":{"maxWidth":1800},"height":"600px","dataKey":"a67967484955196dc36bc167b34ff5e9"},"children":[]},"class":"reactR_markup"},"evals":[],"jsHooks":[]}</script>
</div>
<div id="Parameters" class="tabcontent">
<div id="htmlwidget-3" class="reactable html-widget" style="width:auto;height:600px;"></div>
<script type="application/json" data-for="htmlwidget-3">{"x":{"tag":{"name":"Reactable","attribs":{"data":{"parameter":[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"value":[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null]},"columns":[{"id":"parameter","name":"parameter","type":"logical"},{"id":"value","name":"value","type":"logical"}],"sortable":false,"searchable":true,"pagination":false,"highlight":true,"bordered":true,"striped":true,"style":{"maxWidth":1800},"height":"600px","dataKey":"0b8053400ba14f40add5694cabec5db3"},"children":[]},"class":"reactR_markup"},"evals":[],"jsHooks":[]}</script>
</div>
<div id="Example" class="tabcontent">
<div id="htmlwidget-4" class="reactable html-widget" style="width:auto;height:600px;"></div>
<script type="application/json" data-for="htmlwidget-4">{"x":{"tag":{"name":"Reactable","attribs":{"data":{"person_id":["P0001","P0002","P0002","P0003","P0004","P0005","P0006","P0007","P0007",null,null,null,null,null,null,null,null,null,null,null],"op_meaning":["meaningsHOSP","meaningsHOSP","meaningsHOSP","meaningsHOSP","meaningsHOSP","meaningsHOSP","meaningsHOSP","meaningsHOSP","meaningsHOSP",null,null,null,null,null,null,null,null,null,null,null],"num_spell":[1,1,2,1,1,1,1,1,2,"NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA"],"entry_spell_category":["1997-10-22T00:00:00Z","2016-12-08T00:00:00Z","2018-10-29T00:00:00Z","2006-12-18T00:00:00Z","1983-04-02T00:00:00Z","2016-08-14T00:00:00Z","2019-12-06T00:00:00Z","2016-06-02T00:00:00Z","2017-07-13T00:00:00Z",null,null,null,null,null,null,null,null,null,null,null],"exit_spell_category":["9999-12-31T00:00:00Z","2018-02-08T00:00:00Z","2021-06-02T00:00:00Z","9999-12-31T00:00:00Z","9999-12-31T00:00:00Z","9999-12-31T00:00:00Z","9999-12-31T00:00:00Z","2016-11-16T00:00:00Z","9999-12-31T00:00:00Z",null,null,null,null,null,null,null,null,null,null,null]},"columns":[{"id":"person_id","name":"person_id","type":"character"},{"id":"op_meaning","name":"op_meaning","type":"character"},{"id":"num_spell","name":"num_spell","type":"numeric"},{"id":"entry_spell_category","name":"entry_spell_category","type":"Date"},{"id":"exit_spell_category","name":"exit_spell_category","type":"Date"}],"sortable":false,"searchable":true,"pagination":false,"highlight":true,"bordered":true,"striped":true,"style":{"maxWidth":1800},"height":"600px","dataKey":"1d671048efaf265d8800a2ce89fbea7a"},"children":[]},"class":"reactR_markup"},"evals":[],"jsHooks":[]}</script>
</div>
