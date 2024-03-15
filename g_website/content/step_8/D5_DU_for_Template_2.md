---
weight: 2
name_excel: "D5_DU_for_Template_2.xlsx"
description: "numbers to be included in Template 2:  Description of the time period between MS diagnosis (MS diagnosis date=the date on which the algorithm becomes positive) and pregnancy in D3_DU_PREGNANCY-COHORT_variables if after or during MS diagnosis"
slug: "D5_DU_for_Template_2"
datetime: 1.7105212e+09
title: D5_DU_for_Template_2
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
<button class="tablinks" onclick="openCity(event, &#39;Shell Table&#39;)">Shell Table</button>
</div>
<div id="Metadata" class="tabcontent">
<div id="htmlwidget-1" class="reactable html-widget " style="width:auto;height:600px;"></div>
<script type="application/json" data-for="htmlwidget-1">{"x":{"tag":{"name":"Reactable","attribs":{"data":{"medatata_name":["Name of the dataset","Content of the dataset","Unit of observation","Dataset where the list of UoOs is fully listed and with 1 record per UoO","How many observations per UoO","NxUoO","Variables capturing the UoO","Primary key","Parameters",null,null,null,null,null,null,null,null,null,null,null],"metadata_content":["D5_DU_for_Template_2","numbers to be included in Template 2:  Description of the time period between MS diagnosis (MS diagnosis date=the date on which the algorithm becomes positive) and pregnancy in D3_DU_PREGNANCY-COHORT_variables if after or during MS diagnosis","only one unit: the stratum of pregnancies in the study where the woman has MS ever during the study period","itself","1","1","column_identifier","column_identifier",null,null,null,null,null,null,null,null,null,null,null,null]},"columns":[{"id":"medatata_name","name":"medatata_name","type":"character"},{"id":"metadata_content","name":"metadata_content","type":"character"}],"sortable":false,"searchable":true,"pagination":false,"highlight":true,"bordered":true,"striped":true,"style":{"maxWidth":1800},"height":"600px","dataKey":"5cbb9df8c6da28637170fbc70e3dd2a6"},"children":[]},"class":"reactR_markup"},"evals":[],"jsHooks":[]}</script>
</div>
<div id="Data Model" class="tabcontent">
<div id="htmlwidget-2" class="reactable html-widget " style="width:auto;height:600px;"></div>
<script type="application/json" data-for="htmlwidget-2">{"x":{"tag":{"name":"Reactable","attribs":{"data":{"Variable":["column_identifier","n1","n2","n3","n4","n5",null,null,null,null,null,null,null,null,null,null,null,null,null,null],"Description":["identifier of the column in the shell table Template 1","number of pregnancies in the stratum","number of pregnancies in the stratum whose woman was diagnosed more than 12 months prior to pregnancy","number of pregnancies in the stratum whose woman was diagnosed 3-12 months prior to pregnancy","number of pregnancies in the stratum whose woman was diagnosed 0-3 months prior to pregnancy","number of pregnancies in the stratum whose woman was diagnosed  during pregnancy",null,null,null,null,null,null,null,null,null,null,null,null,null,null],"Format":["int","int","int","int","int","int",null,null,null,null,null,null,null,null,null,null,null,null,null,null],"Vocabulary":["1 = pregnancies in the study where the woman has MS ever during the study",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"Parameters":[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"Notes and examples":[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"Source tables and variables":["D3_DU_MS-PREGNANCY-COHORT_variables","D3_DU_MS-PREGNANCY-COHORT_variables","D3_DU_MS-PREGNANCY-COHORT_variables","D3_DU_MS-PREGNANCY-COHORT_variables","D3_DU_MS-PREGNANCY-COHORT_variables","D3_DU_MS-PREGNANCY-COHORT_variables",null,null,null,null,null,null,null,null,null,null,null,null,null,null],"Retrieved":[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"Created":["yes","yes","yes","yes","yes","yes",null,null,null,null,null,null,null,null,null,null,null,null,null,null],"Algorithm_id":[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"Rule":["1 = all pregnacies in D3_DU_PREGNANCY-COHORT_variables having has_MS_ever == 1","count pregnacies in D3_DU_PREGNANCY-COHORT_variables having has_MS_ever == 1","count pregnacies in D3_DU_PREGNANCY-COHORT_variables having has_MS_ever == 1 and pregnancy_with_MS_detail = \"long before pregnancy\"","count pregnacies in D3_DU_PREGNANCY-COHORT_variables having has_MS_ever == 1 and pregnancy_with_MS_detail = \"recently before pregnancy\"","count pregnacies in D3_DU_PREGNANCY-COHORT_variables having has_MS_ever == 1 and pregnancy_with_MS_detail = right before pregnancy\"","count pregnacies in D3_DU_PREGNANCY-COHORT_variables having has_MS_ever == 1 and (pregnancy_with_MS_detail = \"during pregnancy\" or pregnancy_with_MS_detail = \"right after pregnancy\")",null,null,null,null,null,null,null,null,null,null,null,null,null,null]},"columns":[{"id":"Variable","name":"Variable","type":"character"},{"id":"Description","name":"Description","type":"character"},{"id":"Format","name":"Format","type":"character"},{"id":"Vocabulary","name":"Vocabulary","type":"character"},{"id":"Parameters","name":"Parameters","type":"logical"},{"id":"Notes and examples","name":"Notes and examples","type":"logical"},{"id":"Source tables and variables","name":"Source tables and variables","type":"character"},{"id":"Retrieved","name":"Retrieved","type":"logical"},{"id":"Created","name":"Created","type":"character"},{"id":"Algorithm_id","name":"Algorithm_id","type":"logical"},{"id":"Rule","name":"Rule","type":"character"}],"sortable":false,"searchable":true,"pagination":false,"highlight":true,"bordered":true,"striped":true,"style":{"maxWidth":1800},"height":"600px","dataKey":"2a82f65f83b4959f8c0025d97df4e8ab"},"children":[]},"class":"reactR_markup"},"evals":[],"jsHooks":[]}</script>
</div>
<div id="Parameters" class="tabcontent">
<div id="htmlwidget-3" class="reactable html-widget " style="width:auto;height:600px;"></div>
<script type="application/json" data-for="htmlwidget-3">{"x":{"tag":{"name":"Reactable","attribs":{"data":{"Parameter":[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"Value":[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null]},"columns":[{"id":"Parameter","name":"Parameter","type":"logical"},{"id":"Value","name":"Value","type":"logical"}],"sortable":false,"searchable":true,"pagination":false,"highlight":true,"bordered":true,"striped":true,"style":{"maxWidth":1800},"height":"600px","dataKey":"5a3e224ffdd66e81b7737d629c65f7ec"},"children":[]},"class":"reactR_markup"},"evals":[],"jsHooks":[]}</script>
</div>
<div id="Example" class="tabcontent">
<div id="htmlwidget-4" class="reactable html-widget " style="width:auto;height:600px;"></div>
<script type="application/json" data-for="htmlwidget-4">{"x":{"tag":{"name":"Reactable","attribs":{"data":{"n1_1":[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null]},"columns":[{"id":"n1_1","name":"n1_1","type":"logical"}],"sortable":false,"searchable":true,"pagination":false,"highlight":true,"bordered":true,"striped":true,"style":{"maxWidth":1800},"height":"600px","dataKey":"36b1b30723994782ff84b75dcf7bc4df"},"children":[]},"class":"reactR_markup"},"evals":[],"jsHooks":[]}</script>
</div>
<div id="Shell Table" class="tabcontent">
<div id="htmlwidget-5" class="reactable html-widget " style="width:auto;height:600px;"></div>
<script type="application/json" data-for="htmlwidget-5">{"x":{"tag":{"name":"Reactable","attribs":{"data":{"Date of MS diagnosis ":[null,"More than 12 months prior to pregnancy","3-12 months prior to pregnancy","0-3 months prior to pregnancy","during pregnancy",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"Number of pregnancies N (%)":["n_1","n_2","n_3","n_4","n_5",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null]},"columns":[{"id":"Date of MS diagnosis ","name":"Date of MS diagnosis ","type":"character"},{"id":"Number of pregnancies N (%)","name":"Number of pregnancies N (%)","type":"character"}],"sortable":false,"searchable":true,"pagination":false,"highlight":true,"bordered":true,"striped":true,"style":{"maxWidth":1800},"height":"600px","dataKey":"be2bb39e5444e145ecba28b3adf8da34"},"children":[]},"class":"reactR_markup"},"evals":[],"jsHooks":[]}</script>
</div>
