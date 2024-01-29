---
weight: 9
name_excel: "D3_algorithms_MS.xlsx"
description: "contains the dates when each person in the study population becomes positive for MS according to any of the 5 algorithms"
slug: "D3_algorithms_MS"
datetime: 1.7065268e+09
title: D3_algorithms_MS
author: ''
date: '2024-01-29'
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
<div id="htmlwidget-1" class="reactable html-widget " style="width:auto;height:600px;"></div>
<script type="application/json" data-for="htmlwidget-1">{"x":{"tag":{"name":"Reactable","attribs":{"data":{"medatata_name":["name of the D3","content of the D3","Unit of observation","Dataset where the list of UoOs is fully listed and with 1 record per UoO","How many observations per UoO","NxUoO","Variables capturing the UoO","Primary key","Parameters",null,null,null,null,null,null,null,null,null,null,null],"metadata_content":["D3_algorithms_MS","contains the dates when each person in the study population becomes positive for MS according to any of the 5 algorithms","a person in the SAP1 study population","D3_study_population_SAP1","as many as the times the person becomes first positive for one of the algorithms (maximum: 5)",">= 0 AND <= 5","person_id algorithm",null,null,null,null,null,null,null,null,null,null,null,null,null]},"columns":[{"id":"medatata_name","name":"medatata_name","type":"character"},{"id":"metadata_content","name":"metadata_content","type":"character"}],"sortable":false,"searchable":true,"pagination":false,"highlight":true,"bordered":true,"striped":true,"style":{"maxWidth":1800},"height":"600px","dataKey":"a998f10942d53583ca6a34d105594b1e"},"children":[]},"class":"reactR_markup"},"evals":[],"jsHooks":[]}</script>
</div>
<div id="Data Model" class="tabcontent">
<div id="htmlwidget-2" class="reactable html-widget " style="width:auto;height:600px;"></div>
<script type="application/json" data-for="htmlwidget-2">{"x":{"tag":{"name":"Reactable","attribs":{"data":{"Variable":["person_id","date","algorithm",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"Description":["unique person identifier","date when the algorithm picks the person as with MS","which algorithm between MS1, ..., MS5",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"Format":["character","date","string",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"Vocabulary":["from cdm persons",null,"MS1\r\nMS2\r\nMS3\r\nMS4\r\nMS5",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"Notes and examples":[null,null,"from Table 3 in the SAP",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"retrieved":[null,null,"from D3_components_MS_SAP1",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"computed":[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null]},"columns":[{"id":"Variable","name":"Variable","type":"character"},{"id":"Description","name":"Description","type":"character"},{"id":"Format","name":"Format","type":"character"},{"id":"Vocabulary","name":"Vocabulary","type":"character"},{"id":"Notes and examples","name":"Notes and examples","type":"character"},{"id":"retrieved","name":"retrieved","type":"character"},{"id":"computed","name":"computed","type":"logical"}],"sortable":false,"searchable":true,"pagination":false,"highlight":true,"bordered":true,"striped":true,"style":{"maxWidth":1800},"height":"600px","dataKey":"f699bc78e9da1f6265db577829157221"},"children":[]},"class":"reactR_markup"},"evals":[],"jsHooks":[]}</script>
</div>
<div id="Parameters" class="tabcontent">
<div id="htmlwidget-3" class="reactable html-widget " style="width:auto;height:600px;"></div>
<script type="application/json" data-for="htmlwidget-3">{"x":{"tag":{"name":"Reactable","attribs":{"data":{"parameter":[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"value":[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null]},"columns":[{"id":"parameter","name":"parameter","type":"logical"},{"id":"value","name":"value","type":"logical"}],"sortable":false,"searchable":true,"pagination":false,"highlight":true,"bordered":true,"striped":true,"style":{"maxWidth":1800},"height":"600px","dataKey":"0b8053400ba14f40add5694cabec5db3"},"children":[]},"class":"reactR_markup"},"evals":[],"jsHooks":[]}</script>
</div>
<div id="Example" class="tabcontent">
<div id="htmlwidget-4" class="reactable html-widget " style="width:auto;height:600px;"></div>
<script type="application/json" data-for="htmlwidget-4">{"x":{"tag":{"name":"Reactable","attribs":{"data":{"person_id":["P000001","P000001","P000002",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"date":["2011-01-08T00:00:00Z","2011-01-16T00:00:00Z","2016-03-23T00:00:00Z",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"algorithm":["M1","M2","M1",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null]},"columns":[{"id":"person_id","name":"person_id","type":"character"},{"id":"date","name":"date","type":"Date"},{"id":"algorithm","name":"algorithm","type":"character"}],"sortable":false,"searchable":true,"pagination":false,"highlight":true,"bordered":true,"striped":true,"style":{"maxWidth":1800},"height":"600px","dataKey":"6cc2fa113f27ffe445a2256b3c012244"},"children":[]},"class":"reactR_markup"},"evals":[],"jsHooks":[]}</script>
</div>
