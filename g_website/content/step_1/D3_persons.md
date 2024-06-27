---
weight: 1
name_excel: "D3_persons.xlsx"
description: "contains the cleaned version of PERSONS, where birth date and death date are reconstituted as dates"
slug: "D3_persons"
datetime: 1.7194827e+09
title: D3_persons
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
<script type="application/json" data-for="htmlwidget-1">{"x":{"tag":{"name":"Reactable","attribs":{"data":{"medatata_name":["name of the D3","content of the D3","Unit of observation","Dataset where the list of UoOs is fully listed and with 1 record per UoO","How many observations per UoO","NxUoO","Variables capturing the UoO","Primary key","Parameters",null,null,null,null,null,null,null,null,null,null,null],"metadata_content":["D3_PERSONS","contains the cleaned version of PERSONS, where birth date and death date are reconstituted as dates","a person in PERSON","PERSONS","unique","1","person_id","person_id",null,null,null,null,null,null,null,null,null,null,null,null]},"columns":[{"id":"medatata_name","name":"medatata_name","type":"character"},{"id":"metadata_content","name":"metadata_content","type":"character"}],"sortable":false,"searchable":true,"pagination":false,"highlight":true,"bordered":true,"striped":true,"style":{"maxWidth":1800},"height":"600px","dataKey":"3eb1fedf2eba5025f589d15b5c174522"},"children":[]},"class":"reactR_markup"},"evals":[],"jsHooks":[]}</script>
</div>
<div id="Data Model" class="tabcontent">
<div id="htmlwidget-2" class="reactable html-widget" style="width:auto;height:600px;"></div>
<script type="application/json" data-for="htmlwidget-2">{"x":{"tag":{"name":"Reactable","attribs":{"data":{"Variable":["person_id","sex_at_instance_creation","birth_month_imputed","birth_day_imputed","death_month_imputed","death_day_imputed","birth_date","death_date",null,null,null,null,null,null,null,null,null,null,null,null],"Description":["unique person identifier","sex at instance creation",null,null,null,null,"date reconstituted from day, month and year","date reconstituted from day, month and year",null,null,null,null,null,null,null,null,null,null,null,null],"Format":["character","character","binary","binary","binary","binary","date","date",null,null,null,null,null,null,null,null,null,null,null,null],"Vocabulary":[null,null,"1 = imputed\n0 = otherwise","1 = imputed\n0 = otherwise","1 = imputed\n0 = otherwise","1 = imputed\n0 = otherwise",null,null,null,null,null,null,null,null,null,null,null,null,null,null],"Notes and examples":["from CDM PERSONS","from CDM PERSONS","if the first observation period starts during birth year, the start of the observation period is used as a birth date, otherwise 30 june","if the first observation period starts during birth year, the start of the observation period is used as a birth date, otherwise 30 june",null,null,"algorithm is in place to handle the case when month and day are missing","algorithm is in place to handle the case when month and day are missing",null,null,null,null,null,null,null,null,null,null,null,null],"retrieved":["yes",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"computed":[null,"yes","yes","yes","yes","yes","yes","yes",null,null,null,null,null,null,null,null,null,null,null,null]},"columns":[{"id":"Variable","name":"Variable","type":"character"},{"id":"Description","name":"Description","type":"character"},{"id":"Format","name":"Format","type":"character"},{"id":"Vocabulary","name":"Vocabulary","type":"character"},{"id":"Notes and examples","name":"Notes and examples","type":"character"},{"id":"retrieved","name":"retrieved","type":"character"},{"id":"computed","name":"computed","type":"character"}],"sortable":false,"searchable":true,"pagination":false,"highlight":true,"bordered":true,"striped":true,"style":{"maxWidth":1800},"height":"600px","dataKey":"660e361bebedd3a6b3f8a8c5a557a79c"},"children":[]},"class":"reactR_markup"},"evals":[],"jsHooks":[]}</script>
</div>
<div id="Parameters" class="tabcontent">
<div id="htmlwidget-3" class="reactable html-widget" style="width:auto;height:600px;"></div>
<script type="application/json" data-for="htmlwidget-3">{"x":{"tag":{"name":"Reactable","attribs":{"data":{"parameter":[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"value":[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null]},"columns":[{"id":"parameter","name":"parameter","type":"logical"},{"id":"value","name":"value","type":"logical"}],"sortable":false,"searchable":true,"pagination":false,"highlight":true,"bordered":true,"striped":true,"style":{"maxWidth":1800},"height":"600px","dataKey":"0b8053400ba14f40add5694cabec5db3"},"children":[]},"class":"reactR_markup"},"evals":[],"jsHooks":[]}</script>
</div>
<div id="Example" class="tabcontent">
<div id="htmlwidget-4" class="reactable html-widget" style="width:auto;height:600px;"></div>
<script type="application/json" data-for="htmlwidget-4">{"x":{"tag":{"name":"Reactable","attribs":{"data":{"person_id":["P0001","P0002","P0003",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"sex_at_instance_creation":["F","M","M",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"birth_month_imputed":[0,0,1,"NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA"],"birth_day_imputed":[0,0,1,"NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA"],"death_day_imputed":[0,0,0,"NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA"],"death_month_imputed":[0,0,0,"NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA"],"birth_date":["1968-03-15","1998-01-01","1946-06-30",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"death_date":[null,null,"2021-02-21",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null]},"columns":[{"id":"person_id","name":"person_id","type":"character"},{"id":"sex_at_instance_creation","name":"sex_at_instance_creation","type":"character"},{"id":"birth_month_imputed","name":"birth_month_imputed","type":"numeric"},{"id":"birth_day_imputed","name":"birth_day_imputed","type":"numeric"},{"id":"death_day_imputed","name":"death_day_imputed","type":"numeric"},{"id":"death_month_imputed","name":"death_month_imputed","type":"numeric"},{"id":"birth_date","name":"birth_date","type":"character"},{"id":"death_date","name":"death_date","type":"character"}],"sortable":false,"searchable":true,"pagination":false,"highlight":true,"bordered":true,"striped":true,"style":{"maxWidth":1800},"height":"600px","dataKey":"f262c900e76e35dcf2c31cd7016d367d"},"children":[]},"class":"reactR_markup"},"evals":[],"jsHooks":[]}</script>
</div>
