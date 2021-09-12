## Final Keywords in Clusters

[Control Cluster](https://github.com/a-wis/GT-migr-forecasts/blob/main/data/GT_data/Control_Cluster.xlsx): flower, grass, Greece, horse, money, mother, rocket, Spain, sport, tree

[Education Cluster](https://github.com/a-wis/GT-migr-forecasts/blob/main/data/GT_data/Education_Cluster.csv): education_uk, school_England, school_uk, student_uk, study_uk, Universities_uk 

[Employment Cluster](https://github.com/a-wis/GT-migr-forecasts/blob/main/data/GT_data/Employment_Cluster.xlsx): jobs_England, jobs_uk, locuri_de_munca_Anglia, locuri_de_munca_uk, munca_Anglia, munca_uk, work_uk

[Housing Cluster](https://github.com/a-wis/GT-migr-forecasts/blob/main/data/GT_data/Housing_Cluster.xlsx): casa_anglia, casa_uk, house_england, house_uk, rent_uk, residence_uk

[Pound Cluster](https://github.com/a-wis/GT-migr-forecasts/blob/main/data/GT_data/Pound_Cluster.xlsx): 1_lira, 1_pound, bani_anglia, bani_uk, British_pound, cat_e_lira, currency_ul, curs_lira_sterlina, curs_valutar_lira_sterlina, curs_valutar_lira, evolutie_curs_lira_sterlina, GBP_to_lei, GBP_to_ron, lei_to_GBP, lei_to_pound, lira_sterlina, money_uk, o_lira, one_pound, pound_sterling, pound_to_lei, pound_to_ron, pret_lira, quid, ron_to_GBP, ron_to_pound, salariu_anglia, salariu_uk, salary_uk

## Keywords Selection

> ### Control Cluster

```
Control keywords
├── flower
├── horse
├── tree
├── rocket
├── money
├── sport
├── Greece
├── Spain
├── mother
├── grass
* Just randomly choosing some keywords with a small chance to be directly related with immigration into the UK searches.
```

> ### Employment Cluster

```
Employment
├── employment
|   ├── direct hypernym
│   |   ├── occupation
│   |   ├── job
│   |   ├── line of work (or line)
|   |   └── career
|   ├── derivationally related form   
|   |   └── work
├── employee
|   ├── direct hypernyms   
|   |   └── worker
|   ├── derivationally related form   
|   |   └── employ
*Exclusion criteria: job names (e.g. teacher, bartender, clerk, admin, gardener, etc.)
```
> ### Education Cluster

```
Education
├── education
|   ├── direct hypernym
|   |   └── activity
|   ├── derivationally related form   
│   |   ├── educational
│   |   ├── educationist
│   |   ├── educationalist
│   |   ├── educate
│   |   ├── instructional
│   |   ├── instruct
│   |   ├── teach
│   |   ├── pedagogic
│   |   ├── pedagogical
|   |   └── didactical
|   ├── direct hyponyms 
│   |   ├── coeducation
│   |   ├── continuing education
│   |   ├── course
│   |   ├── course of study
│   |   ├── course of instruction
│   |   ├── class
│   |   ├── elementary education
│   |   ├── extension
│   |   ├── extension service
│   |   ├── class
│   |   ├── university extension
│   |   ├── extracurricular activity
│   |   ├── higher education
│   |   ├── teach teaching
│   |   ├── work-study program
|   |   └── secondary education
├── student
|   ├── direct hypernyms 
|   |   └── enrolee
|   ├── direct hyponyms 
│   |   ├── art student
│   |   ├── auditor
│   |   ├── catechumen
│   |   ├── neophyte
│   |   ├── collegial
│   |   ├── college man
│   |   ├── college boy
│   |   ├── crammer
│   |   ├── Etonian
│   |   ├── Ivy Leaguer
│   |   ├── law student
│   |   ├── major
│   |   ├── medical student
│   |   ├── medico
│   |   ├── nonreader
│   |   ├── passer
│   |   ├── scholar
│   |   ├── seminarian
│   |   ├── seminarist
│   |   ├── sixth-former
│   |   ├── skipper
│   |   ├── underachiever
│   |   ├── underperformer
│   |   ├── nonchiever
│   |   ├── withdrawer
│   |   ├── Wykehamist
|   |   └── overachiever
|   ├── derivationally related form
│   |   ├── studentship
│   |   ├── study
|   |   └── educate
```

> ### Pound Cluster

```
Pound
├── pound
|   ├── direct hypernym
│   |   ├── British pound
│   |   ├── British pound sterling
│   |   ├── pound sterling
│   |   ├── British monetary unit
│   |   ├── Irish monetary unit
│   |   ├── Irish pound
│   |   ├── Irish punt
│   |   ├── punt
|   |   └── quid
├── currency
|   ├── direct hypernyms  
│   |   ├── monetary system
|   |   └── medium of exchange
|   ├── direct hyponyms
│   |   ├── money
│   |   ├── Eurocurrency
│   |   ├── cash
│   |   ├── hard cash
│   |   ├── hard currency
│   |   ├── paper money
│   |   ├── folding money
│   |   ├── paper currency
│   |   ├── coinage
│   |   ├── mintage
│   |   ├── specie
|   |   └── metal money
* + money conversion related keywords (e.g. pound to lei, lei to pound, etc)
```

```
Housing
├── housing
|   ├── direct hyponyms
│   |   ├── apartment
│   |   ├── flat
│   |   ├── billet
│   |   ├── block
│   |   ├── camp
│   |   ├── condominium
│   |   ├── dwelling
│   |   ├── home
│   |   ├── domicile
│   |   ├── above
│   |   ├── habitation
│   |   ├── dwelling home
│   |   ├── hospice
│   |   ├── hostel
│   |   ├── youth hostel
│   |   ├── student lodging 
│   |   ├── living quarters
│   |   ├── quarters
│   |   ├── mobile home
│   |   ├── manufactured home
│   |   ├── pied-a-terre
│   |   ├── quarterring
│   |   ├── rattrap
│   |   ├── shelter
│   |   ├── trach housing
|   |   └── career
|   ├── derivationally related form   
|   |   └── house
├── rent
├── living expenses
```
