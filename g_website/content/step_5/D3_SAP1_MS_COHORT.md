---
weight: 1
name_excel: "D3_SAP1_MS_COHORT.xlsx"
description: "contains the cohort of study population who have MS based on the algorithm chosen at the end of SAP1"
slug: "D3_SAP1_MS_COHORT"
datetime: 1.7105159e+09
title: D3_SAP1_MS_COHORT
author: ''
date: '2024-03-15'
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
<script type="application/json" data-for="htmlwidget-1">{"x":{"tag":{"name":"Reactable","attribs":{"data":{"medatata_name":["name of the D3","content of the D3","Unit of observation","Dataset where the list of UoOs is fully listed and with 1 record per UoO","How many observations per UoO","NxUoO","Variables capturing the UoO","Primary key","Parameters",null,null,null,null,null,null,null,null,null,null,null],"metadata_content":["D3_SAP1_MS-COHORT","contains the cohort of study population who have MS based on the algorithm chosen at the end of SAP1","a person in the SAP1 study population","D3_study_population_SAP1","maximum one, only if they have MS according to the chosen algorithm",">= 0 AND <= 1","person_id","person_id",null,null,null,null,null,null,null,null,null,null,null,null]},"columns":[{"id":"medatata_name","name":"medatata_name","type":"character"},{"id":"metadata_content","name":"metadata_content","type":"character"}],"sortable":false,"searchable":true,"pagination":false,"highlight":true,"bordered":true,"striped":true,"style":{"maxWidth":1800},"height":"600px","dataKey":"cee3c9a331e30d2304b3636eb7619400"},"children":[]},"class":"reactR_markup"},"evals":[],"jsHooks":[]}</script>
</div>
<div id="Data Model" class="tabcontent">
<div id="htmlwidget-2" class="reactable html-widget" style="width:auto;height:600px;"></div>
<script type="application/json" data-for="htmlwidget-2">{"x":{"tag":{"name":"Reactable","attribs":{"data":{"Variable":["person_id  ","date_MS","entry_spell_category","birth_date","cohort_entry_date","cohort_exit_date",null,null,null,null,null,null,null,null,null,null,null,null,null,null],"Description":["unique person identifier  ","when they are diagnosed with MS according to the chosen algorithm D3_algorithms_MS/date where algorithm == MS_chosen","date when the person starts to be observed in the data source",null,"Date when the person enters the study","Date when the person exits the study",null,null,null,null,null,null,null,null,null,null,null,null,null,null],"Format":["character  ",null,"date",null,"date","date",null,null,null,null,null,null,null,null,null,null,null,null,null,null],"Vocabulary":["from cdm persons  ",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"Parameters":[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"Notes and examples":[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"Retrieved":["yes","yes","yes","yes","yes","yes",null,null,null,null,null,null,null,null,null,null,null,null,null,null],"Created":[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"Algorithm_id":[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"Rule":[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null]},"columns":[{"id":"Variable","name":"Variable","type":"character"},{"id":"Description","name":"Description","type":"character"},{"id":"Format","name":"Format","type":"character"},{"id":"Vocabulary","name":"Vocabulary","type":"character"},{"id":"Parameters","name":"Parameters","type":"logical"},{"id":"Notes and examples","name":"Notes and examples","type":"logical"},{"id":"Retrieved","name":"Retrieved","type":"character"},{"id":"Created","name":"Created","type":"logical"},{"id":"Algorithm_id","name":"Algorithm_id","type":"logical"},{"id":"Rule","name":"Rule","type":"logical"}],"sortable":false,"searchable":true,"pagination":false,"highlight":true,"bordered":true,"striped":true,"style":{"maxWidth":1800},"height":"600px","dataKey":"20d269176ef962ef61dcd0541f6ab909"},"children":[]},"class":"reactR_markup"},"evals":[],"jsHooks":[]}</script>
</div>
<div id="Parameters" class="tabcontent">
<div id="htmlwidget-3" class="reactable html-widget" style="width:auto;height:600px;"></div>
<script type="application/json" data-for="htmlwidget-3">{"x":{"tag":{"name":"Reactable","attribs":{"data":{"Parameter":["MS_chosen",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"Value":["depends on the choice of the chosen algorithm: can be MS_1, …, MS_5",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null]},"columns":[{"id":"Parameter","name":"Parameter","type":"character"},{"id":"Value","name":"Value","type":"character"}],"sortable":false,"searchable":true,"pagination":false,"highlight":true,"bordered":true,"striped":true,"style":{"maxWidth":1800},"height":"600px","dataKey":"ce0b59d831ae6e8e742b7432cc3387af"},"children":[]},"class":"reactR_markup"},"evals":[],"jsHooks":[]}</script>
</div>
<div id="Example" class="tabcontent">
<div id="htmlwidget-4" class="reactable html-widget" style="width:auto;height:600px;"></div>
<script type="application/json" data-for="htmlwidget-4">{"x":{"tag":{"name":"Reactable","attribs":{"data":{"person_id  ":["P00071",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"date_MS":[20120514,"NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA"],"entry_spell_category":[20091123,"NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA"],"birth_date":[19821203,"NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA"],"cohort_entry_date":[20091123,"NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA"],"cohort_exit_date":[20191231,"NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA"]},"columns":[{"id":"person_id  ","name":"person_id  ","type":"character"},{"id":"date_MS","name":"date_MS","type":"numeric"},{"id":"entry_spell_category","name":"entry_spell_category","type":"numeric"},{"id":"birth_date","name":"birth_date","type":"numeric"},{"id":"cohort_entry_date","name":"cohort_entry_date","type":"numeric"},{"id":"cohort_exit_date","name":"cohort_exit_date","type":"numeric"}],"sortable":false,"searchable":true,"pagination":false,"highlight":true,"bordered":true,"striped":true,"style":{"maxWidth":1800},"height":"600px","dataKey":"ea1a5b4ad17dd1b7de460c35eef871f6"},"children":[]},"class":"reactR_markup"},"evals":[],"jsHooks":[]}</script>
</div>