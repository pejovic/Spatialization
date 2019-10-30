---
title: "Pollutant inventory spatialization"
author: 
- "Milutin Pejovic"
- "Petar Bursac"
date: "30 October 2019"
output:   
   html_document:
     keep_md: true
     theme: "simplex"
     highlight: tango
     toc: true
     toc_depth: 5
     toc_float: true
     fig_caption: yes
---





# 1A1 - Energys
This document provides the methodlogy and the main results regarding the spatialization of pollutation inventory.























## 1A1a - Public heat and electricity production









<!--html_preserve--><div id="htmlwidget-0d49b11d953ca91e20ca" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-0d49b11d953ca91e20ca">{"x":{"filter":"none","extensions":["Buttons"],"caption":"<caption>Table 1: sf.1A1a<\/caption>","data":[[11.58,72.52,284.53,27.45,112.17,21.21,23.16,39.29,51.9,23.65,1.17,4.65,120.96,6.55,2.53,7.79,50.71,11.45,40.98,19.25,306.63,45.33,23.1,639,5316.23,6776.2,2664.41,30955.98,24796.19],[0.67,4.17,36.23,1.18,27.88,1.26,0.91,96,0.37,2.52,90.56,14.95,475.2,0.06,0.14,0.19,0.44,0.56,0.24,3.14,1017.78,0.14,22.5,2326,37230.57,65369.9,11683.17,142823.37,122778.37],[0.08,0.9,3.02,0.17,1.9,0.36,0.18,15.78,0.33,0.15,20.71,0.55,9.79,0.06,0.17,0.05,0.68,0.25,0.16,0.35,49.14,0.5,0.29,26.97,417.26,288.15,556.2,495.27,518.18],[0.04,0.39,1.31,0.07,0.83,0.15,0.08,6.85,0.14,0.07,8.99,0.24,4.25,0.03,0.07,0.02,0.3,0.11,0.07,0.15,21.34,0.22,0.13,11.71,181.2,125.13,241.54,215.08,225.02],[0.09,0.54,2.11,0.2,0.83,0.16,0.17,0.29,0.38,0.18,0.01,0.03,0.9,0.05,0.02,0.06,0.38,0.08,0.3,0.14,2.27,1.32,0.17,4.74,30.22,38.59,19.76,200.19,160.58],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],["Beogradske elektrane, TO Banovo brdo","Beogradske elektrane, TO Cerak","Beogradske elektrane, TO Novi Beograd","Beogradske elektrane, TO Miljakovac","Beogradske elektrane, TO Dunav","Beogradske elektrane, TO Medaković","Beogradske elektrane, TO Voždovac","Beogradske elektrane, TO Zemun","Beogradske elektrane, TO Konjarnik","Beogradske elektrane, TO Mirijevo","JKP \"Gradska toplana\" Kruševac CTI","Energana","JKP Toplana Bor","Toplana Niš, Krivi Vir","Toplana Niš, Jug","Novosadska toplana, Istok","Novosadska toplana, Jug","Novosadska toplana, Sever","Novosadska toplana, Zapad","Subotička toplana","Kotlarnica na maticnoj lokaciji","Ogranak Panonske TE-TO - TE-TO Novi Sad","Ogranak Panonske TE-TO - TE-TO Sremska Mitrovica","Ogranak Termoelektrane Nikola Tesla - TE Morava","Ogranak Termoelektrane i kopovi Kostolac - TE Kostolac A","Ogranak Termoelektrane i kopovi Kostolac - TE Kostolac B","Ogranak Termoelektrane Nikola Tesla - TE Kolubara","Ogranak Termoelektrane Nikola Tesla - TENT A","Ogranak Termoelektrane Nikola Tesla - TENT B"],["Beogradski region","Beogradski region","Beogradski region","Beogradski region","Beogradski region","Beogradski region","Beogradski region","Beogradski region","Beogradski region","Beogradski region","Region Šumadije i Zapadne Srbije","Region Južne i Istočne Srbije","Region Južne i Istočne Srbije","Region Južne i Istočne Srbije","Region Južne i Istočne Srbije","Region Vojvodine","Region Vojvodine","Region Vojvodine","Region Vojvodine","Region Vojvodine","Region Šumadije i Zapadne Srbije","Region Vojvodine","Region Vojvodine","Region Šumadije i Zapadne Srbije","Region Južne i Istočne Srbije","Region Južne i Istočne Srbije","Beogradski region","Beogradski region","Beogradski region"],["Grad Beograd","Grad Beograd","Grad Beograd","Grad Beograd","Grad Beograd","Grad Beograd","Grad Beograd","Grad Beograd","Grad Beograd","Grad Beograd","Rasinski okrug","Borski okrug","Borski okrug","Nišavski okrug","Nišavski okrug","Južno-bački okrug","Južno-bački okrug","Južno-bački okrug","Južno-bački okrug","Severno-bački okrug","Šumadijski okrug","Južno-bački okrug","Sremski okrug","Pomoravski okrug","Braničevski okrug","Braničevski okrug","Grad Beograd","Grad Beograd","Grad Beograd"],["Beograd-Čukarica","Beograd-Čukarica","Beograd-Novi Beograd","Beograd-Rakovica","Beograd-Stari Grad","Beograd-Voždovac","Beograd-Voždovac","Beograd-Zemun","Beograd-Zvezdara","Beograd-Zvezdara","Kruševac","Bor","Bor","Niš - Mediana","Niš - Palilula","Novi Sad - grad","Novi Sad - grad","Novi Sad - grad","Novi Sad - grad","Subotica","Kragujevac - grad","Novi Sad - grad","Sremska Mitrovica","Svilajnac","Kostolac","Kostolac","Beograd-Lazarevac","Beograd-Obrenovac","Beograd-Obrenovac"],["Beograd (Čukarica)","Beograd (Čukarica)","Beograd (Novi Beograd)","Beograd (Rakovica)","Beograd (Stari Grad)","Beograd (Voždovac)","Beograd (Voždovac)","Beograd (Zemun)","Beograd (Zvezdara)","Beograd (Zvezdara)","Kruševac","Bor","Bor","Niš (Medijana)","Niš (Palilula)","Novi Sad","Novi Sad","Novi Sad","Novi Sad","Subotica","Kragujevac","Novi Sad","Sremska Mitrovica","Svilajnac","Kostolac","Selo Kostolac","Veliki Crljeni","Obrenovac","Ušće"]],"container":"<table class=\"white-space: nowrap\">\n  <thead>\n    <tr>\n      <th>NOx<\/th>\n      <th>SO2<\/th>\n      <th>PM10<\/th>\n      <th>PM2.5<\/th>\n      <th>NMVOC<\/th>\n      <th>NH3<\/th>\n      <th>Unit<\/th>\n      <th>Quantity<\/th>\n      <th>Type<\/th>\n      <th>...10<\/th>\n      <th>Region<\/th>\n      <th>Area<\/th>\n      <th>Municipality<\/th>\n      <th>Location<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"pageLength":5,"dom":"Bfrtip","buttons":["pageLength"],"searchHighlight":true,"scrollX":true,"scrollY":true,"columnDefs":[{"className":"dt-right","targets":[0,1,2,3,4,5]}],"order":[],"autoWidth":false,"orderClasses":false,"lengthMenu":[5,10,25,50,100]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->




<!--html_preserve--><div id="htmlwidget-46122d7e78e1852368eb" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-46122d7e78e1852368eb">{"x":{"filter":"none","caption":"<caption>Table 2: Summary differences<\/caption>","data":[["1","2","3"],["spatialize","total","diff"],[72456.57,72456.57,0],[384008.49,384008.49,0],[2407.59,2407.59,0],[1045.52,1045.52,0],[464.78,464.78,0],[0,0,0]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>sum<\/th>\n      <th>NOx<\/th>\n      <th>SO2<\/th>\n      <th>PM10<\/th>\n      <th>PM2.5<\/th>\n      <th>NMVOC<\/th>\n      <th>NH3<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"pageLength":5,"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"lengthMenu":[5,10,25,50,100]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->




## 1A1b - Refineries







<!--html_preserve--><div id="htmlwidget-bc9e840dbcbb1782c810" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-bc9e840dbcbb1782c810">{"x":{"filter":"none","extensions":["Buttons"],"caption":"<caption>Table 3: sf.1A1b<\/caption>","data":[[3.2,4.57,162.26,2.88],[0.01,0.02,0.72,0.01],[0.05,0.06,2.29,0.04],[0.05,0.06,2.29,0.04],[0.13,0.19,6.64,0.12],[0,0,0,0],[null,null,null,null],[null,null,null,null],[null,null,null,null],["NIS - Naftna industrija Srbije a.d., Pogon za pripremu i transport nafte i gasa, Elemir","NIS - Naftna industrija Srbije ad, NIS - Petrol, Rafinerija nafte Novi Sad","NIS - Naftna industrija Srbije ad, NIS - Petrol, Rafinerija nafte u Pančevu","Rafinerija nafte  Beograd ad"],["Region Vojvodine","Region Vojvodine","Region Vojvodine","Beogradski region"],["Srednje-banatski okrug","Južno-bački okrug","Južno-banatski okrug","Grad Beograd"],["Zrenjanin","Novi Sad - grad","Pančevo","Beograd-Palilula"],["Elemir","Novi Sad","Pančevo","Beograd (Palilula)"]],"container":"<table class=\"white-space: nowrap\">\n  <thead>\n    <tr>\n      <th>NOx<\/th>\n      <th>SO2<\/th>\n      <th>PM10<\/th>\n      <th>PM2.5<\/th>\n      <th>NMVOC<\/th>\n      <th>NH3<\/th>\n      <th>Unit<\/th>\n      <th>Quantity<\/th>\n      <th>Type<\/th>\n      <th>...10<\/th>\n      <th>Region<\/th>\n      <th>Area<\/th>\n      <th>Municipality<\/th>\n      <th>Location<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"pageLength":5,"dom":"Bfrtip","buttons":["pageLength"],"searchHighlight":true,"scrollX":true,"scrollY":true,"columnDefs":[{"className":"dt-right","targets":[0,1,2,3,4,5]}],"order":[],"autoWidth":false,"orderClasses":false,"lengthMenu":[5,10,25,50,100]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->




<!--html_preserve--><div id="htmlwidget-e5f6d567ae12bfe1acb0" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-e5f6d567ae12bfe1acb0">{"x":{"filter":"none","caption":"<caption>Table 4: Summary differences<\/caption>","data":[["1","2","3"],["spatialize","total","diff"],[172.9,172.9,0],[0.77,0.77,0],[2.44,2.44,0],[2.44,2.44,0],[7.08,7.08,0],[0,0,0]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>sum<\/th>\n      <th>NOx<\/th>\n      <th>SO2<\/th>\n      <th>PM10<\/th>\n      <th>PM2.5<\/th>\n      <th>NMVOC<\/th>\n      <th>NH3<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"pageLength":5,"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"lengthMenu":[5,10,25,50,100]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->



## 1B2aiv - Fugitive emissions from liquid fuels: Refining, storage



<!--html_preserve--><div id="htmlwidget-eabfa80c7dc4ddd95dac" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-eabfa80c7dc4ddd95dac">{"x":{"filter":"none","extensions":["Buttons"],"caption":"<caption>Table 5: sf.1A2aiv<\/caption>","data":[[13.04,18.61,661.16,11.72],[33.69,48.07,1708,30.28],[0.54,0.77,27.27,0.48],[0.23,0.33,11.85,0.21],[10.87,15.51,550.97,9.77],[0.06,0.09,3.03,0.05],[null,null,null,null],[null,null,null,null],[null,null,null,null],["NIS - Naftna industrija Srbije a.d., Pogon za pripremu i transport nafte i gasa, Elemir","NIS - Naftna industrija Srbije ad, NIS - Petrol, Rafinerija nafte Novi Sad","NIS - Naftna industrija Srbije ad, NIS - Petrol, Rafinerija nafte u Pančevu","Rafinerija nafte  Beograd ad"],["Region Vojvodine","Region Vojvodine","Region Vojvodine","Beogradski region"],["Srednje-banatski okrug","Južno-bački okrug","Južno-banatski okrug","Grad Beograd"],["Zrenjanin","Novi Sad - grad","Pančevo","Beograd-Palilula"],["Elemir","Novi Sad","Pančevo","Beograd (Palilula)"]],"container":"<table class=\"white-space: nowrap\">\n  <thead>\n    <tr>\n      <th>NOx<\/th>\n      <th>SO2<\/th>\n      <th>PM10<\/th>\n      <th>PM2.5<\/th>\n      <th>NMVOC<\/th>\n      <th>NH3<\/th>\n      <th>Unit<\/th>\n      <th>Quantity<\/th>\n      <th>Type<\/th>\n      <th>...10<\/th>\n      <th>Region<\/th>\n      <th>Area<\/th>\n      <th>Municipality<\/th>\n      <th>Location<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"pageLength":5,"dom":"Bfrtip","buttons":["pageLength"],"searchHighlight":true,"scrollX":true,"scrollY":true,"columnDefs":[{"className":"dt-right","targets":[0,1,2,3,4,5]}],"order":[],"autoWidth":false,"orderClasses":false,"lengthMenu":[5,10,25,50,100]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->




<!--html_preserve--><div id="htmlwidget-3b5045a0bab41312c51b" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-3b5045a0bab41312c51b">{"x":{"filter":"none","caption":"<caption>Table 6: Summary differences<\/caption>","data":[["1","2","3"],["spatialize","total","diff"],[704.53,704.53,0],[1820.04,1820.04,0],[29.06,29.06,0],[12.62,12.62,0],[587.11,587.11,0],[3.23,3.23,0]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>sum<\/th>\n      <th>NOx<\/th>\n      <th>SO2<\/th>\n      <th>PM10<\/th>\n      <th>PM2.5<\/th>\n      <th>NMVOC<\/th>\n      <th>NH3<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"pageLength":5,"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"lengthMenu":[5,10,25,50,100]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->







## 1B2c - Fugitive emissions: Venting and flaring








<!--html_preserve--><div id="htmlwidget-33132b3a2e904a763bc7" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-33132b3a2e904a763bc7">{"x":{"filter":"none","extensions":["Buttons"],"caption":"<caption>Table 5: sf.1B2c<\/caption>","data":[[0.01,0.01,0.26,0],[0.01,0.01,0.37,0.01],[0,0,0,0],[0,0,0,0],[0,0,0.01,0],[0,0,0,0],[null,null,null,null],[null,null,null,null],[null,null,null,null],["NIS - Naftna industrija Srbije a.d., Pogon za pripremu i transport nafte i gasa, Elemir","NIS - Naftna industrija Srbije ad, NIS - Petrol, Rafinerija nafte Novi Sad","NIS - Naftna industrija Srbije ad, NIS - Petrol, Rafinerija nafte u Pančevu","Rafinerija nafte  Beograd ad"],["Region Vojvodine","Region Vojvodine","Region Vojvodine","Beogradski region"],["Srednje-banatski okrug","Južno-bački okrug","Južno-banatski okrug","Grad Beograd"],["Zrenjanin","Novi Sad - grad","Pančevo","Beograd-Palilula"],["Elemir","Novi Sad","Pančevo","Beograd (Palilula)"]],"container":"<table class=\"white-space: nowrap\">\n  <thead>\n    <tr>\n      <th>NOx<\/th>\n      <th>SO2<\/th>\n      <th>PM10<\/th>\n      <th>PM2.5<\/th>\n      <th>NMVOC<\/th>\n      <th>NH3<\/th>\n      <th>Unit<\/th>\n      <th>Quantity<\/th>\n      <th>Type<\/th>\n      <th>...10<\/th>\n      <th>Region<\/th>\n      <th>Area<\/th>\n      <th>Municipality<\/th>\n      <th>Location<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"pageLength":5,"dom":"Bfrtip","buttons":["pageLength"],"searchHighlight":true,"scrollX":true,"scrollY":true,"columnDefs":[{"className":"dt-right","targets":[0,1,2,3,4,5]}],"order":[],"autoWidth":false,"orderClasses":false,"lengthMenu":[5,10,25,50,100]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->




<!--html_preserve--><div id="htmlwidget-f9cb10488ffcb9b412c6" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-f9cb10488ffcb9b412c6">{"x":{"filter":"none","caption":"<caption>Table 6: Summary differences<\/caption>","data":[["1","2","3"],["spatialize","total","diff"],[0.28,0.28,0],[0.39,0.39,0],[0,0,0],[0,0,0],[0.01,0.01,0],[0,0,0]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>sum<\/th>\n      <th>NOx<\/th>\n      <th>SO2<\/th>\n      <th>PM10<\/th>\n      <th>PM2.5<\/th>\n      <th>NMVOC<\/th>\n      <th>NH3<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"pageLength":5,"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"lengthMenu":[5,10,25,50,100]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->




## 1A1c - Manufacturing of solid fuels








<!--html_preserve--><div id="htmlwidget-d296718f861cb3a0528c" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-d296718f861cb3a0528c">{"x":{"filter":"none","extensions":["Buttons"],"caption":"<caption>Table 7: sf.1A1c<\/caption>","data":[[13.43,1.36,3.78,0,0,0.14,1.64,3.82,203.65],[29.56,0,12.24,0,0,10.14,13.75,0,921.49],[5.59,0,2.01,0,0,0,0,0,849.4],[3.89,0,1.4,0,0,0,0,0,591.35],[0.51,0.05,0.14,0,0,0.01,0.06,0.15,7.76],[0,0,0,0,0,0,0,0,0],[null,null,null,null,null,null,null,null,"<-- seems to be the only one doing processing :"],[null,null,null,null,null,null,null,null,null],[null,null,null,null,null,null,null,null,null],["PEU Resavica, Rudnik mrkog uglja Rembas, Resavica","PEU Resavica, Rudnik mrkog uglja Rembas, Vodna","PEU Resavica, Rudnik mrkog uglja Rembas, Senjski rudnik","PEU Resavica, Ibarski rudnici kamenog uglja - Baljevac , Rudnik Ušće","PEU Resavica, Ibarski rudnici kamenog uglja - Baljevac, Rudnik Jarando","PEU Resavica, Rudnik mrkog uglja Štavalj","PEU Resavica, Rudnik mrkog uglja Soko","PEU Resavica, Rudnik antracita Vrška Čuka","Ogranak RB Kolubara - Prerada"],["Region Šumadije i Zapadne Srbije","Region Šumadije i Zapadne Srbije","Region Šumadije i Zapadne Srbije","Region Šumadije i Zapadne Srbije","Region Šumadije i Zapadne Srbije","Region Šumadije i Zapadne Srbije","Region Južne i Istočne Srbije","Region Južne i Istočne Srbije","Beogradski region"],["Pomoravski okrug","Pomoravski okrug","Pomoravski okrug","Raški okrug","Raški okrug","Zlatiborski okrug","Zaječarski okrug","Zaječarski okrug","Grad Beograd"],["Despotovac","Despotovac","Despotovac","Kraljevo","Raška","Sjenica","Sokobanja","Zaječar","Beograd-Lazarevac"],["Resavica","Resavica","Senjski Rudnik","Ušće","Baljevac","Štavalj","Čitluk","Grljan","Vreoci"]],"container":"<table class=\"white-space: nowrap\">\n  <thead>\n    <tr>\n      <th>NOx<\/th>\n      <th>SO2<\/th>\n      <th>PM10<\/th>\n      <th>PM2.5<\/th>\n      <th>NMVOC<\/th>\n      <th>NH3<\/th>\n      <th>Unit<\/th>\n      <th>Quantity<\/th>\n      <th>Type<\/th>\n      <th>...10<\/th>\n      <th>Region<\/th>\n      <th>Area<\/th>\n      <th>Municipality<\/th>\n      <th>Location<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"pageLength":5,"dom":"Bfrtip","buttons":["pageLength"],"searchHighlight":true,"scrollX":true,"scrollY":true,"columnDefs":[{"className":"dt-right","targets":[0,1,2,3,4,5]}],"order":[],"autoWidth":false,"orderClasses":false,"lengthMenu":[5,10,25,50,100]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->




<!--html_preserve--><div id="htmlwidget-841345a460f897fecb64" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-841345a460f897fecb64">{"x":{"filter":"none","caption":"<caption>Table 8: Summary differences<\/caption>","data":[["1","2","3"],["spatialize","total","diff"],[227.81,227.81,0],[987.17,987.17,0],[856.99,856.99,0],[596.64,596.64,0],[8.68,8.68,0],[0,0,0]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>sum<\/th>\n      <th>NOx<\/th>\n      <th>SO2<\/th>\n      <th>PM10<\/th>\n      <th>PM2.5<\/th>\n      <th>NMVOC<\/th>\n      <th>NH3<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"pageLength":5,"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"lengthMenu":[5,10,25,50,100]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->







## 1B1b - Fugitive emissions from solid fuels: Solid fuel transformation







<!--html_preserve--><div id="htmlwidget-57535f7b8be6a5285ad0" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-57535f7b8be6a5285ad0">{"x":{"filter":"none","extensions":["Buttons"],"caption":"<caption>Table 9: sf.1B1b<\/caption>","data":[[0.53],[0.47],[85.56],[35.75],[4.51],[2.17],["is that the only facility transforming coal ?"],[null],[null],["Ogranak RB Kolubara - Prerada"],["Beogradski region"],["Grad Beograd"],["Beograd-Lazarevac"],["Vreoci"]],"container":"<table class=\"white-space: nowrap\">\n  <thead>\n    <tr>\n      <th>NOx<\/th>\n      <th>SO2<\/th>\n      <th>PM10<\/th>\n      <th>PM2.5<\/th>\n      <th>NMVOC<\/th>\n      <th>NH3<\/th>\n      <th>Unit<\/th>\n      <th>Quantity<\/th>\n      <th>Type<\/th>\n      <th>...10<\/th>\n      <th>Region<\/th>\n      <th>Area<\/th>\n      <th>Municipality<\/th>\n      <th>Location<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"pageLength":5,"dom":"Bfrtip","buttons":["pageLength"],"searchHighlight":true,"scrollX":true,"scrollY":true,"columnDefs":[{"className":"dt-right","targets":[0,1,2,3,4,5]}],"order":[],"autoWidth":false,"orderClasses":false,"lengthMenu":[5,10,25,50,100]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->




<!--html_preserve--><div id="htmlwidget-c821a85266a1d499ccb9" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-c821a85266a1d499ccb9">{"x":{"filter":"none","caption":"<caption>Table 10: Summary differences<\/caption>","data":[["1","2","3"],["spatialize","total","diff"],[0.53,0.53,0],[0.47,0.47,0],[85.56,85.56,0],[35.75,35.75,0],[4.51,4.51,0],[2.17,2.17,0]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>sum<\/th>\n      <th>NOx<\/th>\n      <th>SO2<\/th>\n      <th>PM10<\/th>\n      <th>PM2.5<\/th>\n      <th>NMVOC<\/th>\n      <th>NH3<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"pageLength":5,"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"lengthMenu":[5,10,25,50,100]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->












---
title: "1A1-Energy_rmd.r"
author: "User"
date: "2019-10-30"
---
