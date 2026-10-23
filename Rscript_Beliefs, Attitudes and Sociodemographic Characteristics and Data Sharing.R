---
title: "RScript Beliefs, Attitudes and Sociodemographic Characteristics and Data Sharing"
---

# Installeer nodige packages
required_packages <- c(
    "dplyr","readxl","data.table","haven","tidyr","stringr",
    "gt","ca","factoextra","labelled","FactoMineR",
    "caret","mice","glmnet","MASS", "broom", "nnet")

new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# En laden
lapply(required_packages, library, character.only = TRUE)

# Set working directory
path = #put the filepath
setwd(path)

# Inladen bestanden
data_liss_vertrouwen = read_sav("L_Gezondheidsdata/L_Gezondheidsdata_1.0p.sav")
data_liss_health = read_sav("ch22o_EN_1.0p/ch22o_EN_1.0p.sav")
data_liss_religionandethnicity = read_sav("cr23p_EN_1.0p/cr23p_EN_1.0p.sav")
data_liss_socialintegrationandleisure = read_sav("cs23p_EN_1.0p/cs23p_EN_1.0p.sav")
data_liss_politicsandvalues = read_sav("cv23o_EN_1.0p/cv23o_EN_1.0p.sav")

# Bestanden combineren
data_liss_compleet <- merge(data_liss_vertrouwen,data_liss_health, by = "nomem_encr", all.x = TRUE)
data_liss_compleet <- merge(data_liss_compleet,data_liss_politicsandvalues, by = "nomem_encr", all.x = TRUE)
data_liss_compleet <- merge(data_liss_compleet,data_liss_religionandethnicity, by = "nomem_encr", all.x = TRUE)
data_liss_compleet <- merge(data_liss_compleet,data_liss_socialintegrationandleisure, by = "nomem_encr", all.x = TRUE)

#---------------------------------------------------------------------------------------------------------------------------------------------------------
# Opschonen van de data voor analyses

# Aanmaken verschillende categoriale variabelen obv continue waarden, zodat alle predictors straks categoriaal zijn

# variabele aanmaken voor BMI
data_liss_compleet <- data_liss_compleet %>%
  mutate(
    height_m = as.numeric(as.character(ch22o016)) / 100,
    weight_kg = as.numeric(as.character(ch22o017)),
    BMI = ifelse(!is.na(height_m) & height_m > 0,
                 weight_kg / (height_m^2),
                 NA_real_))

# BMI categorieen en naar factor
data_liss_compleet$BMI[data_liss_compleet$BMI <= 10 |
                         data_liss_compleet$BMI >= 75] <- NA

data_liss_compleet$BMI_category <- cut(
  data_liss_compleet$BMI,
  breaks = c(-Inf, 18.5, 25, 30, Inf),
  labels = c("Underweight", "Healthy weight", "Overweight", "Obese"),
  right = FALSE
)

data_liss_compleet$BMI_category <- factor(data_liss_compleet$BMI_category)

# Zelfde voor kans op stemmen
data_liss_compleet$cv23o243_num <-
  suppressWarnings(as.numeric(as.character(data_liss_compleet$cv23o243)))

data_liss_compleet$votingchance_category <- cut(
  data_liss_compleet$cv23o243_num,
  breaks = c(-Inf, 26, 51, 76, 86, 96, Inf),
  labels = c("0-25", "26-50", "51-75", "76-85", "86-95", "96-100"),
  right = FALSE)

data_liss_compleet$votingchance_category <-
  factor(data_liss_compleet$votingchance_category)

# En uren sport in de week
data_liss_compleet$sporthours_category <- cut(
  as.numeric(as.character(data_liss_compleet$cs23p105)),
  breaks = c(-Inf, 1, 2, 3, 4, 5, Inf),
  labels = c("0-1", ">1<2", ">2<3", ">3<4", ">4<5", "5 or more"),
  right = FALSE
)

data_liss_compleet$sporthours_category <-
  factor(data_liss_compleet$sporthours_category)

# Nog variabele aanmaken voor willingness gebaseerd op v3 (1 = not willing to share, 0 = other)
data_liss_compleet$willingness <- factor(
  ifelse(data_liss_compleet$v3 == 4, 1, 0),
  levels = c(0, 1),
  labels = c("Other", "Not willing")
)
table(data_liss_compleet$v2a, data_liss_compleet$willingness, useNA = "always")

# Lijst variabelen
vars_to_keep <- c(
  "leeftijd", "lftdcat", "cs23p021", "cs23p026", "cs23p011", "cs23p046", "cs23p041", 
  "cs23p051", "cs23p056", "cs23p006", "cs23p016", "cs23p036", "cs23p031", "cs23p526", 
  "cs23p061", "cv23o008", "cv23o012", "gebjaar", "BMI_category", "ch22o104", "cv23o019", 
  "cv23o024", "cv23o018", "cv23o017", "cv23o025", "cv23o027", "cv23o013", "cv23o014", 
  "cv23o015", "cv23o021", "DatumE", "DatumB", "eva2t3", "eva2t5", "eva2t1", "eva2t2", 
  "cs23p063", "cs23p070", "ch22o020", "ch22o229", "ch22o239", "eva2t4", "cv23o053", 
  "cv23o166", "cv23o006", "cv23o335", "opm", "cs23p512", "cs23p104", "cr23p143", 
  "ch22o018", "duur", "oplcat", "cv23o307", "geslacht", "nohouse_encr", "cv23o001", 
  "sporthours_category", "cs23p436", "cs23p001", "cr23p164", "ch22o004", "cv23o308", 
  "votingchance_category", "cs23p480", "brutocat", "cv23o116", "woonvorm", "burgstat", 
  "maandnr", "belbezig", "aantalki", "aantalhh", "evaopm", "cv23o047", "cv23o049", 
  "cv23o048", "cv23o052", "positie", "v1", "v3", "cr23p163", "nomem_encr", "cv23o036", 
  "cv23o041", "cv23o035", "cv23o034", "cv23o042", "cv23o044", "cv23o030", "cv23o031", 
  "cv23o032", "cv23o038", "partner", "ch22o013", "ch22o014", "ch22o015", "ch22o012", 
  "ch22o011", "TijdE", "TijdB", "cr23p162", "v2d", "v2c", "v2b", "v2a", "woning", 
  "sted", "cr23p134", "cr23p079", "cv23o101", "cr23p144", "cr23p135", "willingness", 
  "cs23p451", "cs23p603")

data_liss_compleet <- data_liss_compleet %>%
  dplyr::select(all_of(vars_to_keep))

# Value labels toevoegen voor alle variabelen

value_labels_df <- read_excel(
  #"filepath",
  sheet = "value_labels"
)

value_labels_df <- value_labels_df %>%
  mutate(
    value = trimws(as.character(value)),
    label = as.character(label)
  )

variables <- unique(value_labels_df$variable)

for (var in variables) {
  
  if (var %in% names(data_liss_compleet)) {
    
    map <- value_labels_df %>%
      dplyr::filter(variable == var) %>%
      dplyr::select(value, label)
    
    if (nrow(map) > 0) {
      
      x <- as.character(data_liss_compleet[[var]])
      
      # create mapping vector: value -> label
      lookup <- setNames(map$label, map$value)
      
      # replace values with labels
      x_labeled <- lookup[x]
      
      # fallback: keep original if no match
      x_labeled[is.na(x_labeled)] <- x[is.na(x_labeled)]
      
      # convert to factor
      data_liss_compleet[[var]] <- factor(x_labeled)
    }
  }
}

lissanalyse <- data_liss_compleet %>%
  mutate(across(everything(), as.factor))

# Kopietje
df <- data_liss_compleet

# Variabelnamen aanpassen voor helderheid

rename_df <- readxl::read_excel(
  #"filepath",
  sheet = "variable_labels"
)

rename_df <- rename_df %>%
  mutate(
    old_name = trimws(old_name),
    new_name = gsub(" ", ".", trimws(new_name))
  )

rename_vec <- setNames(rename_df$old_name, rename_df$new_name)

data_liss_compleet <- data_liss_compleet %>%
  dplyr::rename(!!!rename_vec)

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Variabelen relevelen tbv forward selection regression analysis: subset van categoriale variabelen die in analyse worden meegenomen. 
# Inclusiecriteria vooraf toepassen: variabelen met teveel missings weg

lissanalyse <- data_liss_compleet

missing_counts <- colSums(is.na(lissanalyse))
missing_perc <- missing_counts / nrow(lissanalyse) * 100
missing_summary <- data.frame(
  Variable = names(missing_counts),
  MissingCount = missing_counts,
  MissingPercent = missing_perc
)
missing_summary[missing_summary$MissingPercent > 50, ]
missing_summary <- missing_summary[order(-missing_summary$MissingPercent), ]

# Vier variabelen met > 50 % missing values: die haal ik eruit

lissanalyse <- lissanalyse[ , !(names(lissanalyse) %in% c("Can.you.indicate.what.kind.of.health.problems.or.what.kind.of.afflicion.you.are.suffering.from.")) ]
lissanalyse <- lissanalyse[ , !(names(lissanalyse) %in% c("You.have.indicated.previously.that.you.do.not.use.social.media..Can.you.indicate.why.not..For.privacy.reasons")) ]
lissanalyse <- lissanalyse[ , !(names(lissanalyse) %in% c("Which.church.community.or.what.religious.group.is.that.")) ]
lissanalyse <- lissanalyse[ , !(names(lissanalyse) %in% c("If.parliamentary.elections.were.held.today..for.which.party.would.you.vote.")) ]

# Eerst nog 'NA' naar 'Don't know / N/A' 

lissanalyse$Preference.type.of.consent.procedure <- as.character(lissanalyse$Preference.type.of.consent.procedure)
lissanalyse$Preference.type.of.consent.procedure[is.na(lissanalyse$Preference.type.of.consent.procedure)] <- "Don't know / N/A"
lissanalyse$Preference.type.of.consent.procedure <- factor(lissanalyse$Preference.type.of.consent.procedure)

# RELEVEL VARIABLES: referentiecategorieen bepalen
names(lissanalyse) <- make.names(names(lissanalyse))

# Outcome variables
lissanalyse$Preference.type.of.consent.procedure <- relevel(lissanalyse$Preference.type.of.consent.procedure, ref = "Opt-out")
lissanalyse$Willingness.to.share.health.data <- relevel(lissanalyse$Willingness.to.share.health.data, ref = "Other")

# Predictors
lissanalyse$Trust.data.reuse..the.government	<-	relevel(lissanalyse$	Trust.data.reuse..the.government	, ref = 	"A lot of trust")
lissanalyse$Trust.data.reuse..scientific.researchers	<-	relevel(lissanalyse$	Trust.data.reuse..scientific.researchers	, ref = 	"A lot of trust")
lissanalyse$Trust.data.reuse..pharmaceutical.companies.that.produce.medicines	<-	relevel(lissanalyse$	Trust.data.reuse..pharmaceutical.companies.that.produce.medicines	, ref = 	"A lot of trust")
lissanalyse$Trust.data.reuse..health.insurers	<-	relevel(lissanalyse$	Trust.data.reuse..health.insurers	, ref = 	"A lot of trust")
lissanalyse$Provision.of.information.regarding.reuse.of.data	<-	relevel(lissanalyse$	Provision.of.information.regarding.reuse.of.data	, ref = 	"No provision of information")
lissanalyse$Position.in.the.household	<-	relevel(lissanalyse$	Position.in.the.household	, ref = 	"Head of household")
lissanalyse$Number.of.household.members	<-	relevel(lissanalyse$	Number.of.household.members	, ref = 	"Two persons")
lissanalyse$Number.of.children.living.at.home	<-	relevel(lissanalyse$	Number.of.children.living.at.home	, ref = 	"None")
lissanalyse$The.head.of.the.household.lives.with.a.partner	<-	relevel(lissanalyse$	The.head.of.the.household.lives.with.a.partner	, ref = 	"Yes")
lissanalyse$Living.environment.of.household	<-	relevel(lissanalyse$	Living.environment.of.household	, ref = 	"(Un)married living together, no child(ren)")
lissanalyse$Type.of.house.the.houselhold.lives.in	<-	relevel(lissanalyse$	Type.of.house.the.houselhold.lives.in	, ref = 	"Owner occupied house")
lissanalyse$Urbanity.of.place.of.residence	<-	relevel(lissanalyse$	Urbanity.of.place.of.residence	, ref = 	"Moderate urbanity")
lissanalyse$Most.important.occupation	<-	relevel(lissanalyse$	Most.important.occupation	, ref = 	"Performs paid work as an employee")
lissanalyse$This.past.month..I.felt.depressed.and.gloomy	<-	relevel(lissanalyse$	This.past.month..I.felt.depressed.and.gloomy	, ref = 	"Never")
lissanalyse$This.past.month..I.felt.happy	<-	relevel(lissanalyse$	This.past.month..I.felt.happy	, ref = 	"Mostly")
lissanalyse$Do.you.suffer.from.a.long.standing.disease..affliction.or.handicap..or.do.you.suffer.from.the.consequences.of.an.accident.	<-	relevel(lissanalyse$	Do.you.suffer.from.a.long.standing.disease..affliction.or.handicap..or.do.you.suffer.from.the.consequences.of.an.accident.	, ref = 	"No")
lissanalyse$Did.you.physical.health.or.emotional.problems.hinder.your.daily.activities.over.the.past.month..for.instance.in.going.for.a.walk..walking.upstairs..dressing.yourself..washing.yourself..visiting.the.toilet.	<-	relevel(lissanalyse$	Did.you.physical.health.or.emotional.problems.hinder.your.daily.activities.over.the.past.month..for.instance.in.going.for.a.walk..walking.upstairs..dressing.yourself..washing.yourself..visiting.the.toilet.	, ref = 	"Not at all")
lissanalyse$Did.you.spend.any.time.in.hospital.or.a.clinic.over.the.past.12.months.	<-	relevel(lissanalyse$	Did.you.spend.any.time.in.hospital.or.a.clinic.over.the.past.12.months.	, ref = 	"No")
lissanalyse$Did.you.take.out.a.complementary.health.insurance.in.2022.	<-	relevel(lissanalyse$	Did.you.take.out.a.complementary.health.insurance.in.2022.	, ref = 	"No")
lissanalyse$How.would.you.describe.your.health..generally.speaking.	<-	relevel(lissanalyse$	How.would.you.describe.your.health..generally.speaking.	, ref = 	"Good")
lissanalyse$This.past.month..I.felt.very.anxious	<-	relevel(lissanalyse$	This.past.month..I.felt.very.anxious	, ref = 	"Never")
lissanalyse$This.past.month..I.felt.so.down.that.nothing.could.cheer.me.up	<-	relevel(lissanalyse$	This.past.month..I.felt.so.down.that.nothing.could.cheer.me.up	, ref = 	"Never")
lissanalyse$This.past.month..I.felt.calm.and.peaceful	<-	relevel(lissanalyse$	This.past.month..I.felt.calm.and.peaceful	, ref = 	"Mostly")
lissanalyse$BMI	<-	relevel(lissanalyse$	BMI	, ref = 	"Healthy weight")
lissanalyse$What.language.or.languages.did.you.grow.up.speaking..Dutch.	<-	relevel(lissanalyse$	What.language.or.languages.did.you.grow.up.speaking..Dutch.	, ref = 	"Yes")
lissanalyse$Were.you.raised.within.a.certain.faith.	<-	relevel(lissanalyse$	Were.you.raised.within.a.certain.faith.	, ref = 	"No")
lissanalyse$Which.faith.was.that.	<-	relevel(lissanalyse$	Which.faith.was.that.	, ref = 	"Roman Catholic")
lissanalyse$Do.you.see.yourself.as.belonging.to.a.church.community.or.religious.group.	<-	relevel(lissanalyse$	Do.you.see.yourself.as.belonging.to.a.church.community.or.religious.group.	, ref = 	"No")
lissanalyse$To.what.extent.would.you.describe.yourself.as.a.religious.person.	<-	relevel(lissanalyse$	To.what.extent.would.you.describe.yourself.as.a.religious.person.	, ref = 	"Certainly not religious")
lissanalyse$Regardless.of.whether.you.see.yourself.as.religious.or.not..to.what.extent.would.you.describe.yourself.as.a.spiritual.person.	<-	relevel(lissanalyse$	Regardless.of.whether.you.see.yourself.as.religious.or.not..to.what.extent.would.you.describe.yourself.as.a.spiritual.person.	, ref = 	"Certainly not spiritual")
lissanalyse$How.strongly.do.you.feel.that.you.are.Dutch.	<-	relevel(lissanalyse$	How.strongly.do.you.feel.that.you.are.Dutch.	, ref = 	"Fairly strongly")
lissanalyse$How.satisfied.are.you.with.the.amount.of.leisure.time.that.you.have.	<-	relevel(lissanalyse$	How.satisfied.are.you.with.the.amount.of.leisure.time.that.you.have.	, ref = 	"Satisfaction: 8 (/10)")
lissanalyse$Are.you.currently.or.have.you.been.a.member.over.the.past.12.months.of.a.sports.club.	<-	relevel(lissanalyse$	Are.you.currently.or.have.you.been.a.member.over.the.past.12.months.of.a.sports.club.	, ref = 	"No")
lissanalyse$Are.you.currently.or.have.you.been.a.member.over.the.past.12.months.of.a.cultural.association.or.hobby.club.	<-	relevel(lissanalyse$	Are.you.currently.or.have.you.been.a.member.over.the.past.12.months.of.a.cultural.association.or.hobby.club.	, ref = 	"No")
lissanalyse$Are.you.currently.or.have.you.been.a.member.over.the.past.12.months.of.a.trade.union.	<-	relevel(lissanalyse$	Are.you.currently.or.have.you.been.a.member.over.the.past.12.months.of.a.trade.union.	, ref = 	"No")
lissanalyse$Are.you.currently.or.have.you.been.a.member.over.the.past.12.months.of.a.business..professional.or.agrarian.organization.	<-	relevel(lissanalyse$	Are.you.currently.or.have.you.been.a.member.over.the.past.12.months.of.a.business..professional.or.agrarian.organization.	, ref = 	"No")
lissanalyse$Are.you.currently.or.have.you.been.a.member.over.the.past.12.months.of.a.consumers..organization.or.automobile.club.	<-	relevel(lissanalyse$	Are.you.currently.or.have.you.been.a.member.over.the.past.12.months.of.a.consumers..organization.or.automobile.club.	, ref = 	"No")
lissanalyse$Are.you.currently.or.have.you.been.a.member.over.the.past.12.months.of.an.organization.for.humanitarian.aid.or.human.rights.	<-	relevel(lissanalyse$	Are.you.currently.or.have.you.been.a.member.over.the.past.12.months.of.an.organization.for.humanitarian.aid.or.human.rights.	, ref = 	"No")
lissanalyse$Are.you.currently.or.have.you.been.a.member.over.the.past.12.months.of.an.organization.for.environmental.protection..peace.organization.or.animal.rights..organization	<-	relevel(lissanalyse$	Are.you.currently.or.have.you.been.a.member.over.the.past.12.months.of.an.organization.for.environmental.protection..peace.organization.or.animal.rights..organization	, ref = 	"No")
lissanalyse$Are.you.currently.or.have.you.been.a.member.over.the.past.12.months.of.a.religious.or.church.organization.	<-	relevel(lissanalyse$	Are.you.currently.or.have.you.been.a.member.over.the.past.12.months.of.a.religious.or.church.organization.	, ref = 	"No")
lissanalyse$Are.you.currently.or.have.you.been.a.member.over.the.past.12.months.of.a.political.party.	<-	relevel(lissanalyse$	Are.you.currently.or.have.you.been.a.member.over.the.past.12.months.of.a.political.party.	, ref = 	"No")
lissanalyse$Are.you.currently.or.have.you.been.a.member.over.the.past.12.months.of.a.science..education..teachers..or.parents..association.	<-	relevel(lissanalyse$	Are.you.currently.or.have.you.been.a.member.over.the.past.12.months.of.a.science..education..teachers..or.parents..association.	, ref = 	"No")
lissanalyse$Are.you.currently.or.have.you.been.a.member.over.the.past.12.months.of.a.social.society..an.association.for.youth..pensioners.senior.citizens..women..or.friends...clubs	<-	relevel(lissanalyse$	Are.you.currently.or.have.you.been.a.member.over.the.past.12.months.of.a.social.society..an.association.for.youth..pensioners.senior.citizens..women..or.friends...clubs	, ref = 	"No")
lissanalyse$Are.you.currently.or.have.you.been.a.member.over.the.past.12.months.of.other.organizations.that.you.can.freely.join.	<-	relevel(lissanalyse$	Are.you.currently.or.have.you.been.a.member.over.the.past.12.months.of.other.organizations.that.you.can.freely.join.	, ref = 	"No")
lissanalyse$Did.you.perform.any.informal.care.over.the.past.12.months.	<-	relevel(lissanalyse$	Did.you.perform.any.informal.care.over.the.past.12.months.	, ref = 	"No")
lissanalyse$Did.you.personally.receive.informal.care.over.the.past.12.months.on.account.of.a.disease.or.other.affliction.	<-	relevel(lissanalyse$	Did.you.personally.receive.informal.care.over.the.past.12.months.on.account.of.a.disease.or.other.affliction.	, ref = 	"No")
lissanalyse$Do.you.practice.sports.	<-	relevel(lissanalyse$	Do.you.practice.sports.	, ref = 	"No")
lissanalyse$How.many.hours.do.you.spend.on.sports.per.week..on.average.	<-	relevel(lissanalyse$	How.many.hours.do.you.spend.on.sports.per.week..on.average.	, ref = 	">3<4")
lissanalyse$How.often.did.you.make.use.of.social.media.in.the.past.2.months.	<-	relevel(lissanalyse$	How.often.did.you.make.use.of.social.media.in.the.past.2.months.	, ref = 	"Every day")
lissanalyse$With.which.of.the.following.devices.do.you.have.access.to.the.internet..None.of.these.	<-	relevel(lissanalyse$	With.which.of.the.following.devices.do.you.have.access.to.the.internet..None.of.these.	, ref = 	"No")
lissanalyse$In.general..do.you.think.about.what.information.you.share.on.social.media.and.about.who.can.see.this.	<-	relevel(lissanalyse$	In.general..do.you.think.about.what.information.you.share.on.social.media.and.about.who.can.see.this.	, ref = 	"I am always aware of this")
lissanalyse$Do.you.have.the.confidence.that.the.following.social.media.respect.your.privacy..Facebook	<-	relevel(lissanalyse$	Do.you.have.the.confidence.that.the.following.social.media.respect.your.privacy..Facebook	, ref = 	"In principle, yes")
lissanalyse$Are.you.currently.or.have.you.been.a.member.over.the.past.12.months.of.an.organization.for.migrants.	<-	relevel(lissanalyse$	Are.you.currently.or.have.you.been.a.member.over.the.past.12.months.of.an.organization.for.migrants.	, ref = 	"No")
lissanalyse$How.dissatisfied.or.satisfied.are.you..generally.speaking..about.what.the.government.has.done.lately.	<-	relevel(lissanalyse$	How.dissatisfied.or.satisfied.are.you..generally.speaking..about.what.the.government.has.done.lately.	, ref = 	"Neither satisfied nor dissatisfied")
lissanalyse$Do.you.follow.the.news..not.or.hardly	<-	relevel(lissanalyse$	Do.you.follow.the.news..not.or.hardly	, ref = 	"No")
lissanalyse$Are.you.very.interested.in.the.news..fairly.interested.or.not.interested.	<-	relevel(lissanalyse$	Are.you.very.interested.in.the.news..fairly.interested.or.not.interested.	, ref = 	"Fairly interested")
lissanalyse$Are.you.very.interesting.in.political.topics..fairly.interested.or.not.interested.	<-	relevel(lissanalyse$	Are.you.very.interesting.in.political.topics..fairly.interested.or.not.interested.	, ref = 	"Fairly interested")
lissanalyse$Confidence..the.Dutch.government	<-	relevel(lissanalyse$	Confidence..the.Dutch.government	, ref = 	"Confidence: 6 (/10)")
lissanalyse$Confidence..the.Dutch.parliament	<-	relevel(lissanalyse$	Confidence..the.Dutch.parliament	, ref = 	"Confidence: 6 (/10)")
lissanalyse$Confidence..the.legal.system	<-	relevel(lissanalyse$	Confidence..the.legal.system	, ref = 	"Confidence: 6 (/10)")
lissanalyse$Confidence..politicians	<-	relevel(lissanalyse$	Confidence..politicians	, ref = 	"Confidence: 6 (/10)")
lissanalyse$Confidence..political.parties	<-	relevel(lissanalyse$	Confidence..political.parties	, ref = 	"Confidence: 6 (/10)")
lissanalyse$Confidence..European.Parliament	<-	relevel(lissanalyse$	Confidence..European.Parliament	, ref = 	"Confidence: 6 (/10)")
lissanalyse$Confidence..the.media	<-	relevel(lissanalyse$	Confidence..the.media	, ref = 	"Confidence: 6 (/10)")
lissanalyse$Confidence..healthcare	<-	relevel(lissanalyse$	Confidence..healthcare	, ref = 	"Confidence: 6 (/10)")
lissanalyse$Confidence..science	<-	relevel(lissanalyse$	Confidence..science	, ref = 	"Confidence: 6 (/10)")
lissanalyse$Confidence..the.democracy	<-	relevel(lissanalyse$	Confidence..the.democracy	, ref = 	"Confidence: 6 (/10)")
lissanalyse$Satisfaction..the.Dutch.government	<-	relevel(lissanalyse$	Satisfaction..the.Dutch.government	, ref = 	"Satisfaction: 6 (/10)")
lissanalyse$Satisfaction..the.Dutch.parliament	<-	relevel(lissanalyse$	Satisfaction..the.Dutch.parliament	, ref = 	"Satisfaction: 6 (/10)")
lissanalyse$Satisfaction..the.legal.system	<-	relevel(lissanalyse$	Satisfaction..the.legal.system	, ref = 	"Satisfaction: 6 (/10)")
lissanalyse$Satisfaction..politicians	<-	relevel(lissanalyse$	Satisfaction..politicians	, ref = 	"Satisfaction: 6 (/10)")
lissanalyse$Satisfaction..political.parties	<-	relevel(lissanalyse$	Satisfaction..political.parties	, ref = 	"Satisfaction: 6 (/10)")
lissanalyse$Satisfaction..European.Parliament	<-	relevel(lissanalyse$	Satisfaction..European.Parliament	, ref = 	"Satisfaction: 6 (/10)")
lissanalyse$Satisfaction..the.media	<-	relevel(lissanalyse$	Satisfaction..the.media	, ref = 	"Satisfaction: 6 (/10)")
lissanalyse$Satisfaction..healthcare	<-	relevel(lissanalyse$	Satisfaction..healthcare	, ref = 	"Satisfaction: 6 (/10)")
lissanalyse$Satisfaction..science	<-	relevel(lissanalyse$	Satisfaction..science	, ref = 	"Satisfaction: 6 (/10)")
lissanalyse$Satisfaction..the.democracy	<-	relevel(lissanalyse$	Satisfaction..the.democracy	, ref = 	"Satisfaction: 6 (/10)")
lissanalyse$Parliamentarians.do.not.care.about.the.opinions.of.people.like.me	<-	relevel(lissanalyse$	Parliamentarians.do.not.care.about.the.opinions.of.people.like.me	, ref = 	"That is not true")
lissanalyse$Political.parties.are.only.interested.in.my.vote.and.not.in.my.opinion	<-	relevel(lissanalyse$	Political.parties.are.only.interested.in.my.vote.and.not.in.my.opinion	, ref = 	"That is not true")
lissanalyse$People.like.me.have.no.influence.at.all.on.government.policy	<-	relevel(lissanalyse$	People.like.me.have.no.influence.at.all.on.government.policy	, ref = 	"That is not true")
lissanalyse$Politics.sometimes.seems.so.complicated.that.people.like.me.can.hardly.understand.what.is.going.on	<-	relevel(lissanalyse$	Politics.sometimes.seems.so.complicated.that.people.like.me.can.hardly.understand.what.is.going.on	, ref = 	"That is not true")
lissanalyse$Did.you.vote.in.the.most.recent.parliamentary.elections..held.on.17.March.2021.	<-	relevel(lissanalyse$	Did.you.vote.in.the.most.recent.parliamentary.elections..held.on.17.March.2021.	, ref = 	"Yes")
lissanalyse$Where.would.you.place.yourself.on.the.scale.below..where.0.means.left.and.10.means.right.	<-	relevel(lissanalyse$	Where.would.you.place.yourself.on.the.scale.below..where.0.means.left.and.10.means.right.	, ref = 	"5 (left - right)")
lissanalyse$It.is.good.if.society.consists.of.people.from.different.cultures.	<-	relevel(lissanalyse$	It.is.good.if.society.consists.of.people.from.different.cultures.	, ref = 	"Agree")
lissanalyse$Do.you.follow.the.news..in.a.paid.daily.newspaper..digital..app.and.or.on.paper.	<-	relevel(lissanalyse$	Do.you.follow.the.news..in.a.paid.daily.newspaper..digital..app.and.or.on.paper.	, ref = 	"No")
lissanalyse$If.parliamentary.elections.were.held.today..what.is.the.percent.chance.that.you.will.vote.	<-	relevel(lissanalyse$	If.parliamentary.elections.were.held.today..what.is.the.percent.chance.that.you.will.vote.	, ref = 	"96-100")
lissanalyse$For.which.party.did.you.vote.in.the.latest.parliamentary.elections.of.17.March.2021.	<-	relevel(lissanalyse$	For.which.party.did.you.vote.in.the.latest.parliamentary.elections.of.17.March.2021.	, ref = 	"VVD (liberal party)")
lissanalyse$Do.you.follow.the.news..via.social.media.	<-	relevel(lissanalyse$	Do.you.follow.the.news..via.social.media.	, ref = 	"No")

# Voor missings imputen sensitivity analyse

# Eerst demografische kenmerken

#Inkomen
# Eerst nieuwe categorieen
lissanalyse <- lissanalyse %>%
  mutate(
    bruto_group = as.character(case_when(
      Income == "No income" ~ "No income",
      
      Income %in% c(
        "EUR 500 of minder",
        "EUR 501 - EUR 1000",
        "EUR 1001 - EUR 1500"
      ) ~ "EUR 1-1500",
      
      Income %in% c(
        "EUR 1501 - EUR 2000",
        "EUR 2001 - EUR 2500",
        "EUR 2501 - EUR 3000"
      ) ~ "EUR 1501-3000",
      
      Income %in% c(
        "EUR 3001 - EUR 3500",
        "EUR 3501 - EUR 4000",
        "EUR 4001 - EUR 4500"
      ) ~ "EUR 3001-4500",
      
      Income %in% c(
        "EUR 4501 - EUR 5000",
        "EUR 5001 - EUR 7500",
        "More than EUR 7500"
      ) ~ "EUR 4501-7500+",
      
      TRUE ~ NA_character_
    ))
  )

# Tabellen demografische kenmerken - zeggenschapsmodaliteiten

table(lissanalyse$Age.in.categories, lissanalyse$Preference.type.of.consent.procedure, useNA = "always")
chisq.test(lissanalyse$Age.in.categories, lissanalyse$Preference.type.of.consent.procedure) #significant

tbl <- table(lissanalyse$Age.in.categories, lissanalyse$Preference.type.of.consent.procedure, useNA = "always")
chisq <- chisq.test(tbl)
chisq$stdres

# Leeftijd in categorieën
#         Opt-out No preference Opt-in Don't know/N/A
# 15-24      92            26    137   26
# 25-34     144            19    280   15
# 35-44     115            27    313   12
# 45-54     131            23    344   10
# 55-64     201            30    467   21
# 65+       437            94    695   53
# <NA>        0             0      0    0

table(lissanalyse$Gender, lissanalyse$Preference.type.of.consent.procedure, useNA = "always")
chisq.test(lissanalyse$Gender, lissanalyse$Preference.type.of.consent.procedure) #not significant

tbl <- table(lissanalyse$Gender, lissanalyse$Preference.type.of.consent.procedure, useNA = "always")
chisq <- chisq.test(tbl)
chisq$stdres

# Geslacht
#         Opt-out No preference Opt-in Don't know/N/A
# Female     543           115   1198   80
# Male       576           104   1034   57
# Other        1             0      4    0
# <NA>         0             0      0    0

table(lissanalyse$bruto_group, lissanalyse$Preference.type.of.consent.procedure, useNA = "always")
chisq.test(lissanalyse$bruto_group, lissanalyse$Preference.type.of.consent.procedure) #significant

tbl <- table(lissanalyse$bruto_group, lissanalyse$Preference.type.of.consent.procedure, useNA = "always")
chisq <- chisq.test(tbl)
chisq$stdres

# Inkomen
#                   Opt-out Don't know / N/A No preference Opt-in <NA>
#   EUR 1-1500          98               14            21    180    0
#   EUR 1501-3000      183               16            40    375    0
#   EUR 3001-4500      205                6            23    335    0
#   EUR 4501-7500+     170                3            11    274    0
#   No income           64               14            16    125    0
#   <NA>               400               84           108    947    0

table(lissanalyse$Education.level, lissanalyse$Preference.type.of.consent.procedure, useNA = "always")
chisq.test(lissanalyse$Education.level, lissanalyse$Preference.type.of.consent.procedure) #significant

tbl <- table(lissanalyse$Education.level, lissanalyse$Preference.type.of.consent.procedure, useNA = "always")
chisq <- chisq.test(tbl)
chisq$stdres

# Opleidingsniveau
#                 Opt-out No preference Opt-in Don't know/N/A
# Basisonderwijs      48            29    117   33
# HAVO/VWO           108            16    213   15
# HBO                334            38    613   20
# MBO                247            62    591   26
# VMBO               160            53    363   38
# WO                 223            21    339    5
# <NA>                 0             0      0    0

table(lissanalyse$Marital.status, lissanalyse$Preference.type.of.consent.procedure, useNA = "always")
chisq.test(lissanalyse$Marital.status, lissanalyse$Preference.type.of.consent.procedure) #significant

tbl <- table(lissanalyse$Marital.status, lissanalyse$Preference.type.of.consent.procedure, useNA = "always")
chisq <- chisq.test(tbl)
chisq$stdres

# Burgerlijke staat
#                   Opt-out No preference Opt-in Don't know/N/A
# Divorced             121            24    266   11
# Married              570           100   1132   52
# Never married        351            69    714   55
# Seperated              2             0     15    4
# Widow or widower      76            26    109   15
# <NA>                   0             0      0    0

# Tabellen demografische kenmerken - willingness

table(lissanalyse$Age.in.categories, lissanalyse$Willingness.to.share.health.data, useNA = "always")
chisq.test(lissanalyse$Age.in.categories, lissanalyse$Willingness.to.share.health.data) #significant

tbl <- table(lissanalyse$Age.in.categories, lissanalyse$Willingness.to.share.health.data, useNA = "always")
chisq <- chisq.test(tbl)
chisq$stdres

# Leeftijd in categorieën
#         Other Not willing <NA>
# 15-24   254          26    1
# 25-34   411          47    0
# 35-44   400          67    0
# 45-54   432          76    0
# 55-64   606         113    0
# 65+    1076         202    1
# <NA>      0           0    0

table(lissanalyse$Gender, lissanalyse$Willingness.to.share.health.data, useNA = "always")
chisq.test(lissanalyse$Gender, lissanalyse$Willingness.to.share.health.data) #not significant

tbl <- table(lissanalyse$Gender, lissanalyse$Willingness.to.share.health.data, useNA = "always")
chisq <- chisq.test(tbl)
chisq$stdres

# Geslacht
#         Other Not willing <NA>
# Female  1659         277    0
# Male    1516         253    2
# Other      4           1    0
# <NA>       0           0    0

table(lissanalyse$bruto_group, lissanalyse$Willingness.to.share.health.data, useNA = "always")
chisq.test(lissanalyse$bruto_group, lissanalyse$Willingness.to.share.health.data)

tbl <- table(lissanalyse$bruto_group, lissanalyse$Willingness.to.share.health.data, useNA = "always")
chisq <- chisq.test(tbl)
chisq$stdres

# Inkomen
#                 Other Not willing <NA>
# EUR 1-1500       271          42    0
# EUR 1501-3000    540          74    0
# EUR 3001-4500    519          50    0
# EUR 4501-7500+   416          42    0
# No income        185          34    0
# <NA>            1248         289    2

table(lissanalyse$Education.level, lissanalyse$Willingness.to.share.health.data, useNA = "always")
chisq.test(lissanalyse$Education.level, lissanalyse$Willingness.to.share.health.data) #significant

tbl <- table(lissanalyse$Education.level, lissanalyse$Willingness.to.share.health.data, useNA = "always")
chisq <- chisq.test(tbl)
chisq$stdres

# Opleidingsniveau
#                 Other Not willing <NA>
# Basisonderwijs   173          54    0
# HAVO/VWO         311          41    0
# HBO              892         112    1
# MBO              775         150    1
# VMBO             499         115    0
# WO               529          59    0
# <NA>               0           0    0

table(lissanalyse$Marital.status, lissanalyse$Willingness.to.share.health.data, useNA = "always")
chisq.test(lissanalyse$Marital.status, lissanalyse$Willingness.to.share.health.data) #not significant

tbl <- table(lissanalyse$Marital.status, lissanalyse$Willingness.to.share.health.data, useNA = "always")
chisq <- chisq.test(tbl)
chisq$stdres

# Burgerlijke staat
#                   Other Not willing <NA>
# Divorced           353          69    0
# Married           1587         266    1
# Never married     1039         149    1
# Seperated           16           5    0
# Widow or widower   184          42    0
# <NA>                 0           0    0

### Dan forward selection regression analyses

outcome <- "Preference.type.of.consent.procedure"
outcome_two <- "Willingness.to.share.health.data"

# Set met predictors
predictors_sensitivity <- c(
    "Are.you.currently.or.have.you.been.a.member.over.the.past.12.months.of.a.business..professional.or.agrarian.organization.", 
    "Are.you.currently.or.have.you.been.a.member.over.the.past.12.months.of.a.consumers..organization.or.automobile.club.", 
    "Are.you.currently.or.have.you.been.a.member.over.the.past.12.months.of.a.cultural.association.or.hobby.club.", 
    "Are.you.currently.or.have.you.been.a.member.over.the.past.12.months.of.a.political.party.", 
    "Are.you.currently.or.have.you.been.a.member.over.the.past.12.months.of.a.religious.or.church.organization.", 
    "Are.you.currently.or.have.you.been.a.member.over.the.past.12.months.of.a.science..education..teachers..or.parents..association.", 
    "Are.you.currently.or.have.you.been.a.member.over.the.past.12.months.of.a.social.society..an.association.for.youth..pensioners.senior.citizens..women..or.friends...clubs", 
    "Are.you.currently.or.have.you.been.a.member.over.the.past.12.months.of.a.sports.club.", 
    "Are.you.currently.or.have.you.been.a.member.over.the.past.12.months.of.a.trade.union.", 
    "Are.you.currently.or.have.you.been.a.member.over.the.past.12.months.of.an.organization.for.environmental.protection..peace.organization.or.animal.rights..organization", 
    "Are.you.currently.or.have.you.been.a.member.over.the.past.12.months.of.an.organization.for.humanitarian.aid.or.human.rights.", 
    "Are.you.currently.or.have.you.been.a.member.over.the.past.12.months.of.an.organization.for.migrants.", 
    "Are.you.currently.or.have.you.been.a.member.over.the.past.12.months.of.other.organizations.that.you.can.freely.join.", 
    "Are.you.very.interested.in.the.news..fairly.interested.or.not.interested.", 
    "Are.you.very.interesting.in.political.topics..fairly.interested.or.not.interested.", 
    "BMI.categories", 
    "Confidence..European.Parliament", 
    "Confidence..healthcare", 
    "Confidence..political.parties", 
    "Confidence..politicians", 
    "Confidence..science", 
    "Confidence..the.democracy", 
    "Confidence..the.Dutch.government", 
    "Confidence..the.Dutch.parliament", 
    "Confidence..the.legal.system", 
    "Confidence..the.media", 
    "Did.you.perform.any.informal.care.over.the.past.12.months.", 
    "Did.you.personally.receive.informal.care.over.the.past.12.months.on.account.of.a.disease.or.other.affliction.", 
    "Did.you.physical.health.or.emotional.problems.hinder.your.daily.activities.over.the.past.month..for.instance.in.going.for.a.walk..walking.upstairs..dressing.yourself..washing.yourself..visiting.the.toilet.", 
    "Did.you.spend.any.time.in.hospital.or.a.clinic.over.the.past.12.months.", 
    "Did.you.take.out.a.complementary.health.insurance.in.2022.", 
    "Did.you.vote.in.the.most.recent.parliamentary.elections..held.on.17.March.2021.", 
    "Do.you.follow.the.news..in.a.paid.daily.newspaper..digital..app.and.or.on.paper.", 
    "Do.you.follow.the.news..not.or.hardly", 
    "Do.you.follow.the.news..via.social.media.", 
    "Do.you.have.the.confidence.that.the.following.social.media.respect.your.privacy..Facebook", 
    "Do.you.practice.sports.", 
    "Do.you.see.yourself.as.belonging.to.a.church.community.or.religious.group.", 
    "Do.you.suffer.from.a.long.standing.disease..affliction.or.handicap..or.do.you.suffer.from.the.consequences.of.an.accident.", 
    "For.which.party.did.you.vote.in.the.latest.parliamentary.elections.of.17.March.2021.", 
    "How.dissatisfied.or.satisfied.are.you..generally.speaking..about.what.the.government.has.done.lately.", 
    "How.many.hours.do.you.spend.on.sports.per.week..on.average.", 
    "How.often.did.you.make.use.of.social.media.in.the.past.2.months.", 
    "How.satisfied.are.you.with.the.amount.of.leisure.time.that.you.have.", 
    "How.strongly.do.you.feel.that.you.are.Dutch.", 
    "How.would.you.describe.your.health..generally.speaking.", 
    "If.parliamentary.elections.were.held.today..what.is.the.percent.chance.that.you.will.vote.", 
    "In.general..do.you.think.about.what.information.you.share.on.social.media.and.about.who.can.see.this.", 
    "It.is.good.if.society.consists.of.people.from.different.cultures.", 
    "Living.environment.of.household", 
    "Most.important.occupation", 
    "Number.of.children.living.at.home", 
    "Number.of.household.members", 
    "Parliamentarians.do.not.care.about.the.opinions.of.people.like.me", 
    "People.like.me.have.no.influence.at.all.on.government.policy", 
    "Political.parties.are.only.interested.in.my.vote.and.not.in.my.opinion", 
    "Politics.sometimes.seems.so.complicated.that.people.like.me.can.hardly.understand.what.is.going.on", 
    "Position.in.the.household", 
    "Provision.of.information.regarding.reuse.of.data", 
    "Regardless.of.whether.you.see.yourself.as.religious.or.not..to.what.extent.would.you.describe.yourself.as.a.spiritual.person.", 
    "Satisfaction..European.Parliament", 
    "Satisfaction..healthcare", 
    "Satisfaction..political.parties", 
    "Satisfaction..politicians", 
    "Satisfaction..science", 
    "Satisfaction..the.democracy", 
    "Satisfaction..the.Dutch.government", 
    "Satisfaction..the.Dutch.parliament", 
    "Satisfaction..the.legal.system", 
    "Satisfaction..the.media", 
    "The.head.of.the.household.lives.with.a.partner", 
    "This.past.month..I.felt.calm.and.peaceful", 
    "This.past.month..I.felt.depressed.and.gloomy", 
    "This.past.month..I.felt.happy", 
    "This.past.month..I.felt.so.down.that.nothing.could.cheer.me.up", 
    "This.past.month..I.felt.very.anxious", 
    "To.what.extent.would.you.describe.yourself.as.a.religious.person.", 
    "Trust.data.reuse..health.insurers", 
    "Trust.data.reuse..pharmaceutical.companies.that.produce.medicines", 
    "Trust.data.reuse..scientific.researchers", 
    "Trust.data.reuse..the.government", 
    "Type.of.house.the.houselhold.lives.in", 
    "Urbanity.of.place.of.residence", 
    "Were.you.raised.within.a.certain.faith.", 
    "What.language.or.languages.did.you.grow.up.speaking..Dutch.", 
    "Where.would.you.place.yourself.on.the.scale.below..where.0.means.left.and.10.means.right.", 
    "Which.faith.was.that.", 
    "With.which.of.the.following.devices.do.you.have.access.to.the.internet..None.of.these."
)

lissanalyse_pred_sensitivity <- lissanalyse %>%
  dplyr::select(all_of(predictors_sensitivity))

# Dataset voor analyses predictors en outcome, maar NIET de geimputeerde versies
stepwise_data_sensitivity <-
  lissanalyse[, c(outcome, predictors_sensitivity)]
stepwise_data_sensitivity <- na.omit(stepwise_data_sensitivity)

stepwise_data_sensitivity[[outcome]] <-
  relevel(
    as.factor(stepwise_data_sensitivity[[outcome]]),
    ref = "Opt-out"
  )

base_model <- nnet::multinom(
  as.formula(paste(outcome, "~ 1")),
  data = stepwise_data_sensitivity,
  trace = FALSE
)

full_formula <- as.formula(
  paste(outcome, "~", paste(predictors_sensitivity, collapse = " + "))
)

step_model <- stepAIC(
  base_model,
  scope = list(lower = ~1, upper = full_formula),
  direction = "forward",
  trace = TRUE
)

summary(step_model)

coefs <- summary(step_model)$coefficients
ses   <- summary(step_model)$standard.errors

z_vals <- coefs / ses

p_vals <- 2 * (1 - pnorm(abs(z_vals)))

p_table <- as.data.frame(p_vals)

p_table_long <- p_table %>%
  mutate(outcome_category = rownames(p_table)) %>%
  tidyr::pivot_longer(
    cols = -outcome_category,
    names_to = "predictor",
    values_to = "p_value"
  ) %>%
  filter(p_value < 0.05)

# En herhalen voor outcome two

stepwise_data_sensitivity <-
  lissanalyse[, c(outcome_two, predictors_sensitivity)]
stepwise_data_sensitivity <- na.omit(stepwise_data_sensitivity)

stepwise_data_sensitivity[[outcome_two]] <-
  relevel(
    as.factor(stepwise_data_sensitivity[[outcome_two]]),
    ref = "Other"
  )

base_model <- nnet::multinom(
  as.formula(paste(outcome_two, "~ 1")),
  data = stepwise_data_sensitivity,
  trace = FALSE
)

full_formula <- as.formula(
  paste(outcome_two, "~", paste(predictors_sensitivity, collapse = " + "))
)

step_model <- stepAIC(
  base_model,
  scope = list(lower = ~1, upper = full_formula),
  direction = "forward",
  trace = TRUE
)

summary(step_model)

coefs <- summary(step_model)$coefficients
ses   <- summary(step_model)$standard.errors

z_vals <- coefs / ses

p_vals <- 2 * (1 - pnorm(abs(z_vals)))

p_table <- as.data.frame(p_vals)

p_table_long_two <- p_table %>%
  mutate(outcome_category = rownames(p_table)) %>%
  tidyr::pivot_longer(
    cols = -outcome_category,
    names_to = "predictor",
    values_to = "p_value"
  ) %>%
  filter(p_value < 0.05)

# Voor analyses missings imputen

colSums(is.na(lissanalyse))

# Had al eerder > 50% gedropped: nu <5%: drop de values(de cases in geval van missing), en >5%-<50% impute de values

# percentage missing per variabele berekenen
missing_percent <- colSums(is.na(lissanalyse)) / nrow(lissanalyse) * 100
# selecteer variabelen met <5% missing
vars_lt5 <- names(missing_percent[missing_percent < 5])
# behoudt variabelen met 0% missing, maar wordt TRUE bij >0 en <5 %, en verwijderd rij (case) in geval van TRUE
lissanalyse_clean <- lissanalyse[complete.cases(lissanalyse[, vars_lt5]), ]
# aantal overgebleven rijen
nrow(lissanalyse_clean)

# MICE imputation uitvoeren, eerst bruto_group eruit halen 
exclude_var <- "bruto_group"
dat <- lissanalyse_clean

# Alleen predictors voor mice meenemen, maar wel ID, 2 outcomes en ook bruto_group excluderen
predictors <- predictors_sensitivity
dat_mice <- dat[, c("Respondent.ID", outcome, outcome_two, predictors)]

# bruto_group niet meenemen
dat_mice <- dat_mice %>%
  dplyr::select(-any_of(exclude_var))

# MICE als methode gebruiken
meth <- make.method(dat_mice)

# Variabelen zonder missings niet meenemen
meth[colSums(is.na(dat_mice)) == 0] <- ""

# Numeriek met predictive mean matching
meth[sapply(dat_mice, is.numeric)] <- "pmm"

# Binair met logistic regression
meth[sapply(dat_mice, function(x) is.factor(x) && nlevels(x) == 2)] <- "logreg"

# Categoriaal ook met pmm
meth[sapply(dat_mice, function(x) is.factor(x) && nlevels(x) > 2)] <- "pmm"

pred <- make.predictorMatrix(dat_mice)

# ID niet als predictor
pred[, "Respondent.ID"] <- 0

# ID niet imputen
pred["Respondent.ID", ] <- 0

meth["Respondent.ID"] <- ""

# Geen self-prediction
diag(pred) <- 0

# Model formaat iets reduceren ivm 'too many weights', max 10 pred. per variabele
for (i in 1:nrow(pred)) {
  vars <- which(pred[i, ] == 1)
  if (length(vars) > 10) {
    keep <- sample(vars, 10)
    pred[i, ] <- 0
    pred[i, keep] <- 1
  }
}

# Daadwerkelijk imputeren
imp <- mice(
  dat_mice,
  m = 5,
  maxit = 10,
  method = meth,
  predictorMatrix = pred,
  seed = 123
)

# Geimputeerde dataset
completed_data <- complete(imp, 1)

completed_data$bruto_group <- lissanalyse_clean$bruto_group[
  match(completed_data$Respondent.ID,
        lissanalyse_clean$Respondent.ID)
]

# bruto_group weer toevoegen (achteraf zodat niet ingevuld ook echt leeg blijft)
completed_data$bruto_group <- lissanalyse_clean$bruto_group[
  match(completed_data$Respondent.ID, lissanalyse_clean$Respondent.ID)
]

# Vanaf hier alleen 'completed_data' gebruiken als dataset

# Volgende stap univariate screening

# Outcome vaststellen
outcome <- "Preference.type.of.consent.procedure" 

# Predictors lijst
predictors <- setdiff(names(completed_data), outcome)

# Univariate screening resultaten opzet
univar_results <- data.frame(
  Predictor_Var = character(),
  Predictor_LevelName = character(),
  Outcome = character(),
  Coefficient = numeric(),
  StdError = numeric(),
  z_value = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

# En de daadwerkelijke screening
for (pred in predictors) {
  formula_uni <- as.formula(paste(outcome, "~", pred))
  fit <- tryCatch(multinom(formula_uni, data = completed_data, trace = FALSE),
                  error = function(e) { warning(paste("skipping", pred, ":", e$message)); return(NULL) })
  if (is.null(fit)) next
  
  s <- summary(fit)
  coef_mat <- s$coefficients
  se_mat   <- s$standard.errors
  z_mat    <- coef_mat / se_mat
  p_mat    <- 2 * (1 - pnorm(abs(z_mat)))
  
  for (cat in rownames(coef_mat)) {
    for (colnm in colnames(coef_mat)) {
      level_label <- sub(paste0("^", pred), "", colnm)
      level_label <- sub("^\\.|^:|^_", "", level_label)
      level_label <- trimws(level_label)
      univar_results <- rbind(univar_results, data.frame(
        Predictor_Var = pred,
        Predictor_LevelName = ifelse(nchar(level_label)==0, colnm, level_label),
        Outcome = cat,
        Coefficient = coef_mat[cat, colnm],
        StdError = se_mat[cat, colnm],
        z_value = z_mat[cat, colnm],
        p_value = p_mat[cat, colnm],
        stringsAsFactors = FALSE
      ))
    }
  }
}

# Subset van screening (p < 0.20)
significant_vars <- subset(univar_results, p_value < 0.20)
head(significant_vars, 20)

# Intercepten weghalen
significant_vars_clean <- subset(significant_vars, Predictor_LevelName != "(Intercept)")

# Check
nrow(significant_vars_clean)
head(significant_vars_clean)

write.csv(significant_vars_clean, "Univariate_Screening_Results_23juni2026.csv", row.names = FALSE)

unique(significant_vars$Predictor_Var)

# Vanaf hier forward selection regression analysis / stepwise regression (met de gefilterde significante predictor levels van bovenstaande dataframe)

outcome <- "Preference.type.of.consent.procedure"

# Set met predictors
candidate_predictors <- unique(significant_vars_clean$Predictor_Var)

# Dataset voor analyses alleen geimputeerde predictors en outcomes
stepwise_data <- completed_data[, c(outcome, candidate_predictors)]

stepwise_data <- na.omit(stepwise_data)

stepwise_data[[outcome]] <- relevel(as.factor(stepwise_data[[outcome]]), ref = "Opt-out")

base_model <- multinom(as.formula(paste(outcome, "~ 1")), data = stepwise_data, trace = FALSE)

full_formula <- as.formula(paste(outcome, "~", paste(candidate_predictors, collapse = " + ")))

# Analyse
step_model <- stepAIC(base_model,
                      scope = list(lower = ~1, upper = full_formula),
                      direction = "forward",
                      trace = TRUE)

summary(step_model)

# Model summary
s <- summary(step_model)

# Extract coefficienten en SEs
coef_mat <- s$coefficients
se_mat   <- s$standard.errors

# Z-values en p-values berekenen
z_mat <- coef_mat / se_mat
p_mat <- 2 * (1 - pnorm(abs(z_mat)))

# Dataframe maken met alle waarden
results_df <- as.data.frame(as.table(coef_mat)) %>%
  rename(Outcome = Var1, Predictor_Level = Var2, Coefficient = Freq) %>%
  mutate(
    StdError = as.vector(se_mat),
    z_value = as.vector(z_mat),
    p_value = as.vector(p_mat)
  )

# Intercepten weg
results_df <- results_df %>% filter(Predictor_Level != "(Intercept)")

head(results_df)

# Excel van maken
write.csv(results_df, "Stepwise_Multinom_Results_23juni2026.csv", row.names = FALSE)

# OR en 95% confidence intervals
results_df <- results_df %>%
  mutate(
    OR = exp(Coefficient),
    OR_low = exp(Coefficient - 1.96 * StdError),
    OR_high = exp(Coefficient + 1.96 * StdError)
  )

# Afronden
results_df <- results_df %>%
  mutate(
    Coefficient = round(Coefficient, 3),
    StdError = round(StdError, 3),
    z_value = round(z_value, 3),
    p_value = signif(p_value, 3),
    OR = round(OR, 3),
    OR_low = round(OR_low, 3),
    OR_high = round(OR_high, 3)
  )

write.csv(results_df, "Stepwise_Multinom_Results_with_OR2_23juni2026.csv", row.names = FALSE)
head(results_df)

# Toevoegen aantal respondenten en percentage respondenten dat corresponderen met uitkomsten

# Weet niet zeker of dit moet
mm <- model.matrix(step_model, data = stepwise_data)
yvec <- as.character(stepwise_data[[outcome]])

# Berekenen
n_row <- nrow(results_df)
n_cell <- integer(n_row)
pct_of_outcome <- numeric(n_row)

# Zodat de variabelnamen juist worden behandeld
escape_regex <- function(x) gsub("([.|()\\^{}+$*?\\[\\]\\\\])","\\\\\\1", x)

for(i in seq_len(n_row)) {
  pl <- results_df$Predictor_Level[i]
  out_cat <- as.character(results_df$Outcome[i])
  
  # Columnmatch
  colname <- NULL
  if(pl %in% colnames(mm)) {
    colname <- pl
  } else {
    pat <- paste0("^", escape_regex(pl), "$")
    cand <- grep(pat, colnames(mm), value = TRUE, perl = TRUE)
    if(length(cand) == 0) {
      cand <- grep(pl, colnames(mm), value = TRUE, fixed = TRUE)
    }
    if(length(cand) > 0) colname <- cand[1]
  }
  
  if(is.null(colname)) {
    n_cell[i] <- NA_integer_
    pct_of_outcome[i] <- NA_real_
  } else {
    sel <- (mm[, colname] == 1) & (yvec == out_cat)
    n_cell[i] <- sum(sel, na.rm = TRUE)
    total_outcome <- sum(yvec == out_cat, na.rm = TRUE)
    pct_of_outcome[i] <- if(total_outcome > 0) 100 * n_cell[i] / total_outcome else NA_real_
  }
}

# Toevoegen aan resultaten
results_df$n_cell <- n_cell
results_df$pct_of_outcome <- round(pct_of_outcome, 2)

# Check 
head(results_df)

write.csv(results_df, "Stepwise_Multinom_Results_with_OR3_23juni2026.csv", row.names = FALSE)

#-------------------------------------------------------------------------------------------------------------------
# Hier hetzelfde herhalen maar dan voor willingness to share healthdata

# Volgende stap univariate screening

# Outcome bepalen
outcome_parttwo <- "Willingness.to.share.health.data"

# Als factor
completed_data[[outcome_parttwo]] <- relevel(
  as.factor(completed_data[[outcome_parttwo]]),
  ref = "Other"
)

# Predictors list
predictors_parttwo <- setdiff(
  names(completed_data),
  c(
    outcome_parttwo,
    "Provision.of.information.regarding.reuse.of.data",
    "Preference.type.of.consent.procedure"
  )
)

# Data frame
univar_results_parttwo <- data.frame(
  Predictor_Var = character(),
  Predictor_LevelName = character(),
  Outcome = character(),
  Coefficient = numeric(),
  StdError = numeric(),
  z_value = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

# Analyse
for (pred in predictors_parttwo) {
  formula_uni_parttwo <- as.formula(paste(outcome_parttwo, "~", pred))
  
  fit <- tryCatch(glm(formula_uni_parttwo, data = completed_data, family = binomial),
                  error = function(e) { 
                    warning(paste("skipping", pred, ":", e$message))
                    return(NULL) 
                  })
  if (is.null(fit)) next
  
  s <- summary(fit)
  coef_tbl <- coef(summary(fit))
  
  # Voor in dataframe
  for (colnm in rownames(coef_tbl)) {
    level_label <- sub(paste0("^", pred), "", colnm)
    level_label <- sub("^\\.|^:|^_", "", level_label)
    level_label <- trimws(level_label)
    
    univar_results_parttwo <- rbind(univar_results_parttwo, data.frame(
      Predictor_Var = pred,
      Predictor_LevelName = ifelse(nchar(level_label)==0, colnm, level_label),
      Outcome = paste0("vs ", levels(completed_data[[outcome_parttwo]])[1]),  # reference: predicted category
      Coefficient = coef_tbl[colnm, "Estimate"],
      StdError = coef_tbl[colnm, "Std. Error"],
      z_value = coef_tbl[colnm, "z value"],
      p_value = coef_tbl[colnm, "Pr(>|z|)"],
      stringsAsFactors = FALSE
    ))
  }
}

# Filter voor p < 0.20
significant_vars_parttwo <- subset(univar_results_parttwo, p_value < 0.20)

# Intercepten weg
significant_vars_clean_parttwo <- subset(significant_vars_parttwo, Predictor_LevelName != "(Intercept)")

# Checks
nrow(significant_vars_clean_parttwo)
head(significant_vars_clean_parttwo, 20)

write.csv(significant_vars_clean_parttwo, "Univariate_Screening_Results_Parttwo_23juni2026.csv", row.names = FALSE)

unique(significant_vars_clean_parttwo$Predictor_Var)

# Vanaf hier forward selection regression analysis / stepwise regression (met de gefilterde significante predictor levels van bovenstaande dataframe)

outcome_parttwo <- "Willingness.to.share.health.data"

candidate_predictors_parttwo <- unique(significant_vars_clean_parttwo$Predictor_Var)

# Set van predictors + outcome
stepwise_data_parttwo <- completed_data[, c(outcome_parttwo, candidate_predictors_parttwo)]

# Check!
nrow(stepwise_data_parttwo)

stepwise_data_parttwo <- na.omit(stepwise_data_parttwo)
base_model_parttwo <- glm(as.formula(paste(outcome_parttwo, "~ 1")), data = stepwise_data_parttwo, family = binomial)
full_formula_parttwo <- as.formula(paste(outcome_parttwo, "~", paste(candidate_predictors_parttwo, collapse = " + ")))

step_model_parttwo <- stepAIC(base_model_parttwo,
                              scope = list(lower = ~1, upper = full_formula_parttwo),
                              direction = "forward",
                              trace = TRUE)

summary(step_model_parttwo)

s <- summary(step_model_parttwo)

# Coefficienten in dataframe
coef_tbl <- as.data.frame(s$coefficients)
colnames(coef_tbl) <- c("Coefficient", "StdError", "z_value", "p_value")

# Variabelnamen toevoegen
coef_tbl <- coef_tbl %>%
  tibble::rownames_to_column("Predictor_LevelName") %>%
  mutate(
    Predictor_Var = sub("\\..*", "", Predictor_LevelName),
    Outcome = levels(stepwise_data_parttwo[[outcome_parttwo]])[2]
  )

# Odds ratios
coef_tbl <- coef_tbl %>%
  mutate(OddsRatio = exp(Coefficient))

results_df_parttwo <- coef_tbl
head(results_df_parttwo)

# 95% confidence intervals
results_df_parttwo <- results_df_parttwo %>%
  mutate(
    OR_low = exp(Coefficient - 1.96 * StdError),
    OR_high = exp(Coefficient + 1.96 * StdError)
  )

# Afronden
results_df_parttwo <- results_df_parttwo %>%
  mutate(
    Coefficient = round(Coefficient, 3),
    StdError = round(StdError, 3),
    z_value = round(z_value, 3),
    p_value = signif(p_value, 3),
    OddsRatio = round(OddsRatio, 3),
    OR_low = round(OR_low, 3),
    OR_high = round(OR_high, 3)
  )

# Toevoegen aantal respondenten en percentage respondenten dat corresponderen met uitkomsten

outcome_parttwo <- "Willingness.to.share.health.data"

mm2 <- model.matrix(step_model_parttwo, data = stepwise_data_parttwo)
yvec2 <- as.character(stepwise_data_parttwo[[outcome_parttwo]])

# Counts en percentages
n_row <- nrow(results_df_parttwo)
n_cell <- integer(n_row)
pct_of_outcome <- numeric(n_row)

# Voor string issues
escape_regex <- function(x) gsub("([.|()\\^{}+$*?\\[\\]\\\\])","\\\\\\1", x)

for(i in seq_len(n_row)) {
  pl <- results_df_parttwo$Predictor_LevelName[i]
  out_cat <- as.character(results_df_parttwo$Outcome[i])
  
  colname <- NULL
  if(pl %in% colnames(mm2)) {
    colname <- pl
  } else {
    pat <- paste0("^", escape_regex(pl), "$")
    cand <- grep(pat, colnames(mm2), value = TRUE, perl = TRUE)
    if(length(cand) == 0) {
      cand <- grep(pl, colnames(mm2), value = TRUE, fixed = TRUE)
    }
    if(length(cand) > 0) colname <- cand[1]
  }
  
  if(is.null(colname)) {
    n_cell[i] <- NA_integer_
    pct_of_outcome[i] <- NA_real_
  } else {
    sel <- (mm2[, colname] == 1) & (yvec2 == out_cat)
    n_cell[i] <- sum(sel, na.rm = TRUE)
    total_outcome <- sum(yvec2 == out_cat, na.rm = TRUE)
    pct_of_outcome[i] <- if(total_outcome > 0) 100 * n_cell[i] / total_outcome else NA_real_
  }
}

# Bij DF
results_df_parttwo$n_cell <- n_cell
results_df_parttwo$pct_of_outcome <- round(pct_of_outcome, 2)
head(results_df_parttwo)

write.csv(results_df_parttwo, "Stepwise_Multinom_Results_with_OR_parttwo_23juni2026.csv", row.names = FALSE)

#------------Tabel met overlap tussen wil niet delen en consent system

table(completed_data$Preference.type.of.consent.procedure, completed_data$Willingness.to.share.health.data)

#------------Tabel met demografische kenmerken laten uitdraaien door R (non-missing populatie)

table(completed_data$Preference.type.of.consent.procedure)
table(completed_data$lftdcat, completed_data$Preference.type.of.consent.procedure)
table(completed_data$geslacht, completed_data$Preference.type.of.consent.procedure)
table(completed_data$brutocat, completed_data$Preference.type.of.consent.procedure)
table(completed_data$oplcat, completed_data$Preference.type.of.consent.procedure)
table(completed_data$burgstat, completed_data$Preference.type.of.consent.procedure)

table(completed_data$Preference.type.of.consent.procedure)
table(completed_data$lftdcat, completed_data$Willingness.to.share.health.data)
table(completed_data$geslacht, completed_data$Willingness.to.share.health.data)
table(completed_data$brutocat, completed_data$Willingness.to.share.health.data)
table(completed_data$oplcat, completed_data$Willingness.to.share.health.data)
table(completed_data$burgstat, completed_data$Willingness.to.share.health.data)

#---------------------------------------------------------------------------------------------------------------------

# Tabellen demografische kenmerken - zeggenschapsmodaliteiten

table(lissanalyse_clean$Age.in.categories, lissanalyse_clean$Preference.type.of.consent.procedure, useNA = "always")
chisq.test(lissanalyse_clean$Age.in.categories, lissanalyse_clean$Preference.type.of.consent.procedure) #significant

tbl <- table(lissanalyse_clean$Age.in.categories, lissanalyse_clean$Preference.type.of.consent.procedure, useNA = "always")
chisq <- chisq.test(tbl)
chisq$stdres

# Leeftijd
#           Opt-out Don't know / N/A No preference Opt-in <NA>
#   15-24      88               17            22    128    0
#   25-34     139               14            17    267    0
#   35-44     115                6            24    299    0
#   45-54     129                5            21    324    0
#   55-64     197               14            27    438    0
#   65+       424               37            82    649    0
#   <NA>        0                0             0      0    0

table(lissanalyse_clean$Gender, lissanalyse_clean$Preference.type.of.consent.procedure, useNA = "always")
chisq.test(lissanalyse_clean$Gender, lissanalyse_clean$Preference.type.of.consent.procedure) #not significant

tbl <- table(lissanalyse_clean$Gender, lissanalyse_clean$Preference.type.of.consent.procedure, useNA = "always")
chisq <- chisq.test(tbl)
chisq$stdres

# Geslacht
#           Opt-out Don't know / N/A No preference Opt-in <NA>
#   Female     525               53           103   1114    0
#   Male       566               40            90    987    0
#   Other        1                0             0      4    0
#   <NA>         0                0             0      0    0

table(lissanalyse_clean$bruto_group, lissanalyse_clean$Preference.type.of.consent.procedure, useNA = "always")
chisq.test(lissanalyse_clean$bruto_group, lissanalyse_clean$Preference.type.of.consent.procedure) #significant

tbl <- table(lissanalyse_clean$bruto_group, lissanalyse_clean$Preference.type.of.consent.procedure, useNA = "always")
chisq <- chisq.test(tbl)
chisq$stdres

# Inkomen
#                   Opt-out Don't know / N/A No preference Opt-in <NA>
#   EUR 1-1500          95                9            18    171    0
#   EUR 1501-3000      181               11            36    351    0
#   EUR 3001-4500      202                6            21    327    0
#   EUR 4501-7500+     168                2            11    265    0
#   No income           63                6            14    115    0
#   <NA>               383               59            93    876    0
  
table(lissanalyse_clean$Education.level, lissanalyse_clean$Preference.type.of.consent.procedure, useNA = "always")
chisq.test(lissanalyse_clean$Education.level, lissanalyse_clean$Preference.type.of.consent.procedure) #significant

tbl <- table(lissanalyse_clean$Education.level, lissanalyse_clean$Preference.type.of.consent.procedure, useNA = "always")
chisq <- chisq.test(tbl)
chisq$stdres

# Opleidingsniveau
#                   Opt-out Don't know / N/A No preference Opt-in <NA>
#   Basisonderwijs      44               23            23    103    0
#   HAVO/VWO           107               11            13    203    0
#   HBO                329               16            37    583    0
#   MBO                238               16            56    554    0
#   VMBO               153               22            44    331    0
#   WO                 221                5            20    331    0
#   <NA>                 0                0             0      0    0

table(lissanalyse_clean$Marital.status, lissanalyse_clean$Preference.type.of.consent.procedure, useNA = "always")
chisq.test(lissanalyse_clean$Marital.status, lissanalyse_clean$Preference.type.of.consent.procedure) #significant

tbl <- table(lissanalyse_clean$Marital.status, lissanalyse_clean$Preference.type.of.consent.procedure, useNA = "always")
chisq <- chisq.test(tbl)
chisq$stdres

# Burgerlijke staat
#                     Opt-out Don't know / N/A No preference Opt-in <NA>
#   Divorced             116                8            19    247    0
#   Married              562               33            92   1067    0
#   Never married        340               40            59    676    0
#   Seperated              2                2             0     15    0
#   Widow or widower      72               10            23    100    0
#   <NA>                   0                0             0      0    0

# Tabellen demografische kenmerken - willingness

table(lissanalyse_clean$Age.in.categories, lissanalyse_clean$Willingness.to.share.health.data, useNA = "always")
chisq.test(lissanalyse_clean$Age.in.categories, lissanalyse_clean$Willingness.to.share.health.data) #significant

tbl <- table(lissanalyse_clean$Age.in.categories, lissanalyse_clean$Willingness.to.share.health.data, useNA = "always")
chisq <- chisq.test(tbl)
chisq$stdres

# Leeftijd
#         Other Not willing <NA>
# 15-24   234          21    0
# 25-34   394          43    0
# 35-44   387          57    0
# 45-54   409          70    0
# 55-64   576         100    0
# 65+    1014         178    0
# <NA>      0           0    0

table(lissanalyse_clean$Gender, lissanalyse_clean$Willingness.to.share.health.data, useNA = "always")
chisq.test(lissanalyse_clean$Gender, lissanalyse_clean$Willingness.to.share.health.data) #not significant

tbl <- table(lissanalyse_clean$Gender, lissanalyse_clean$Willingness.to.share.health.data, useNA = "always")
chisq <- chisq.test(tbl)
chisq$stdres

# Geslacht
#         Other Not willing <NA>
# Female  1557         238    0
# Male    1453         230    0
# Other      4           1    0
# <NA>       0           0    0

table(lissanalyse_clean$bruto_group, lissanalyse_clean$Willingness.to.share.health.data, useNA = "always")
chisq.test(lissanalyse_clean$bruto_group, lissanalyse_clean$Willingness.to.share.health.data) #not significant

tbl <- table(lissanalyse_clean$bruto_group, lissanalyse_clean$Willingness.to.share.health.data, useNA = "always")
chisq <- chisq.test(tbl)
chisq$stdres

# Inkomen
#                 Other Not willing <NA>
# EUR 1-1500       259          34    0
# EUR 1501-3000    511          68    0
# EUR 3001-4500    509          47    0
# EUR 4501-7500+   405          41    0
# No income        172          26    0
# <NA>            1158         253    0

table(lissanalyse_clean$Education.level, lissanalyse_clean$Willingness.to.share.health.data, useNA = "always")
chisq.test(lissanalyse_clean$Education.level, lissanalyse_clean$Willingness.to.share.health.data) #significant

tbl <- table(lissanalyse_clean$Education.level, lissanalyse_clean$Willingness.to.share.health.data, useNA = "always")
chisq <- chisq.test(tbl)
chisq$stdres

# Opleidingsniveau
#                 Other Not willing <NA>
# Basisonderwijs   152          41    0
# HAVO/VWO         296          38    0
# HBO              862         103    0
# MBO              731         133    0
# VMBO             455          95    0
# WO               518          59    0
# <NA>               0           0    0

table(lissanalyse_clean$Marital.status, lissanalyse_clean$Willingness.to.share.health.data, useNA = "always")
chisq.test(lissanalyse_clean$Marital.status, lissanalyse_clean$Willingness.to.share.health.data) #not significant

tbl <- table(lissanalyse_clean$Marital.status, lissanalyse_clean$Willingness.to.share.health.data, useNA = "always")
chisq <- chisq.test(tbl)
chisq$stdres

# Burgerlijke staat
#                   Other Not willing <NA>
# Divorced           329          61    0
# Married           1515         239    0
# Never married      982         133    0
# Seperated           15           4    0
# Widow or widower   173          32    0
# <NA>                 0           0    0

# ------------ Open tekst opmerkingen

open_answers_table <- data_liss_compleet %>%
  filter(
    !is.na(Open.text.remarks),
    Open.text.remarks != ""
  ) %>%
  dplyr::select(Open.text.remarks, Gender, Age)

write.csv(
  open_answers_table,
  "Open_Answers_Gender_Age.csv",
  row.names = FALSE
)
