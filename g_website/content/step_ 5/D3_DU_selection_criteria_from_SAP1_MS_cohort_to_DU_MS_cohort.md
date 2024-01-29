---
weight: 2
name_excel: "D3_DU_selection_criteria_from_SAP1_MS_cohort_to_DU_MS_cohort.xlsx"
description: "contains the exclusion criteria to go from D3_study_population_SAP1 to the study population of the DP3 DU, as decsribed in subsection 4.3.1 of MS DP3_SAP_DU_MS_V2.2"
slug: "D3_DU_selection_criteria_from_SAP1_MS_cohort_to_DU_MS_cohort"
datetime: 1.7065269e+09
title: D3_DU_selection_criteria_from_SAP1_MS_cohort_to_DU_MS_cohort
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
<script type="application/json" data-for="htmlwidget-1">{"x":{"tag":{"name":"Reactable","attribs":{"data":{"medatata_name":["name of the D3","content of the D3","Unit of observation","Dataset where the list of UoOs is fully listed and with 1 record per UoO","How many observations per UoO","NxUoO","Variables capturing the UoO","Primary key","Parameters",null,null,null,null,null,null,null,null,null,null,null],"metadata_content":["D3_DU_selection_criteria_from_SAP1_MS_cohort_to_DU_MS_cohort","contains the exclusion criteria to go from D3_study_population_SAP1 to the study population of the DP3 DU, as decsribed in subsection 4.3.1 of MS DP3_SAP_DU_MS_V2.2","a person in the SAP1 study population","D3_study_population_SAP1","1","1","person_id","person_id",null,null,null,null,null,null,null,null,null,null,null,null]},"columns":[{"id":"medatata_name","name":"medatata_name","type":"character"},{"id":"metadata_content","name":"metadata_content","type":"character"}],"sortable":false,"searchable":true,"pagination":false,"highlight":true,"bordered":true,"striped":true,"style":{"maxWidth":1800},"height":"600px","dataKey":"6ff0523102290c1434b3fa83b31ff58b"},"children":[]},"class":"reactR_markup"},"evals":[],"jsHooks":[]}</script>
</div>
<div id="Data Model" class="tabcontent">
<div id="htmlwidget-2" class="reactable html-widget " style="width:auto;height:600px;"></div>
<script type="application/json" data-for="htmlwidget-2">{"x":{"tag":{"name":"Reactable","attribs":{"data":{"Variable":["person_id  ","never_positive_for_MS_chosen","women_diagnosed_outside_childbearing_age","women_with_less_than_1_year_fup",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"Description":["unique person identifier  ","excludes persons who are  never positive for MS_chosen during their study period, that is, who are not stored in D3_SAP1_MS-COHORT","excludes persons which are < 15 years old and > 50 at D3_SAP1_MS-COHORT/date_MS","excludes women who are diagnosed <1 year before D3_SAP1_MS-COHORT/cohort_exit_date",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"Format":["character  ","binary","binary","binary",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"Vocabulary":["from cdm persons  ","0 = positive at some point\r\n1 = otherwise\r\n","0 = outside of childbearing age\r\n1 = otherwise\r\n","0 = less than year of follow up at diagnosis date\r\n1 = otherwise\r\n",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"Parameters":[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"Notes and examples":[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"Source tables and variables":[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"Retrieved":["yes",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"Created":[null,"yes","yes","yes",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"Algorithm_id":[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"Rule":[null,null,null,"D3_SAP1_MS-COHORT/cohort_exit_date - D3_SAP1_MS-COHORT/date_MS < 365",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null]},"columns":[{"id":"Variable","name":"Variable","type":"character"},{"id":"Description","name":"Description","type":"character"},{"id":"Format","name":"Format","type":"character"},{"id":"Vocabulary","name":"Vocabulary","type":"character"},{"id":"Parameters","name":"Parameters","type":"logical"},{"id":"Notes and examples","name":"Notes and examples","type":"logical"},{"id":"Source tables and variables","name":"Source tables and variables","type":"logical"},{"id":"Retrieved","name":"Retrieved","type":"character"},{"id":"Created","name":"Created","type":"character"},{"id":"Algorithm_id","name":"Algorithm_id","type":"logical"},{"id":"Rule","name":"Rule","type":"character"}],"sortable":false,"searchable":true,"pagination":false,"highlight":true,"bordered":true,"striped":true,"style":{"maxWidth":1800},"height":"600px","dataKey":"7117aa9c39a3f797f95262bcf9252d23"},"children":[]},"class":"reactR_markup"},"evals":[],"jsHooks":[]}</script>
</div>
<div id="Parameters" class="tabcontent">
<div id="htmlwidget-3" class="reactable html-widget " style="width:auto;height:600px;"></div>
<script type="application/json" data-for="htmlwidget-3">{"x":{"tag":{"name":"Reactable","attribs":{"data":{"Parameter":[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"Value":[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null]},"columns":[{"id":"Parameter","name":"Parameter","type":"logical"},{"id":"Value","name":"Value","type":"logical"}],"sortable":false,"searchable":true,"pagination":false,"highlight":true,"bordered":true,"striped":true,"style":{"maxWidth":1800},"height":"600px","dataKey":"5a3e224ffdd66e81b7737d629c65f7ec"},"children":[]},"class":"reactR_markup"},"evals":[],"jsHooks":[]}</script>
</div>
<div id="Example" class="tabcontent">
<div id="htmlwidget-4" class="reactable html-widget " style="width:auto;height:600px;"></div>
<script type="application/json" data-for="htmlwidget-4">{"x":{"tag":{"name":"Reactable","attribs":{"data":{"person_id  ":[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null]},"columns":[{"id":"person_id  ","name":"person_id  ","type":"logical"}],"sortable":false,"searchable":true,"pagination":false,"highlight":true,"bordered":true,"striped":true,"style":{"maxWidth":1800},"height":"600px","dataKey":"056919339959cfe7c4ff44cb678f96ae"},"children":[]},"class":"reactR_markup"},"evals":[],"jsHooks":[]}</script>
</div>
