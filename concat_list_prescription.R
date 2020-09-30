#-------------------------------------------------------------------------------------------------------------------------------------------------------
# This is the function for writing fertilization prescription
#-------------------------------------------------------------------------------------------------------------------------------------------------------

prescription_properties<-list()

prescription_properties[["Boron_not_likely"]] <-
 "In most cases, the addition of B to the fertilizer prescription is likely not needed. However, on sites subject to frequent and prolonged growing season moisture deficits, the inclusion of 2-3 kg B/ha (as granular borate [15% B]) in the fertilizer prescription will eliminate the small risk of N-induced B deficiency."

prescription_properties[["Boron_possible"]]<-
 "Foliar diagnosis indicates B deficiency may be induced by N fertilization, especially on coarse-textured soils subject to growing season moisture deficits. The inclusion of 2-3 kg B/ha (as granular borate [15% B]) in the fertilizer prescription may be warranted."

prescription_properties[["Boron_likely"]]<-
 "Foliar diagnosis indicates B deficiency will likely be induced or exacerbated by N fertilization. It is recommended that 2-3 kg B/ha (as granular borate [15% B]) be included in the fertilizer prescription."

prescription_properties[["Boron_severe"]]<-
 "Foliar diagnosis indicates B is severely deficient. It is strongly recommended that 2-3 kg B/ha (as granular borate [15% B]) be included in the fertilizer prescription."

prescription_properties[["Calcium_Deficiency_severe_or_moderate"]]<-
 "Foliar diagnosis indicates Ca is deficient. Growth response of lodgepole pine following Ca fertilization in the B.C. interior is not well documented. It is recommended that a tree nutrition specialist be consulted before including Ca in the fertilizer prescription."

prescription_properties[["Copper_Deficiency_probable"]]<-
 "Foliar diagnosis indicates Cu deficiency is probable. Growth response of lodgepole pine following Cu fertilization in the B.C. interior is not well documented. It is recommended that a tree nutrition specialist be consulted before including Cu in the fertilizer prescription."

prescription_properties[["Iron_Deficiency_probable"]]<-
 "Foliar diagnosis indicates Fe deficiency is probable. Growth response of lodgepole pine following Fe fertilization in the B.C. interior is not well documented. It is recommended that a tree nutrition specialist be consulted before including Fe in the fertilizer prescription."

prescription_properties[["Macronutrients_and_Ratio_slight"]]<-
 "Available research information indicates that slight to moderate macronutrient (P, K, Ca, Mg) deficiencies do not prevent favourable responses to N fertilization. Therefore, the addition of these nutrients to the fertilizaer prescription is currently not recommended."

prescription_properties[["Magnesium_Deficiency_severe_or_moderate"]]<-
 "Foliar diagnosis indicates Mg is deficient. Growth response of lodgepole pine following Mg fertilization in the B.C. interior is not well documented. It is recommended that a tree nutrition specialist be consulted before including Mg in the fertilizer prescription."

prescription_properties[["Magnesium_Ratio_Deficiency_severe_or_moderate"]]<-
 "Foliar diagnosis indicates N/Mg is deficient. Growth response of lodgepole pine following Mg fertilization in the B.C. interior is not well documented. It is recommended that a tree nutrition specialist be consulted before including Mg in the fertilizer prescription."

prescription_properties[["Manganese_Deficiency_probable"]]<-
 "Foliar diagnosis indicates Mn deficiency is probable. Growth response of lodgepole pine following Mn fertilization in the B.C. interior is not well documented. It is recommended that a tree nutrition specialist be consulted before including Mn in the fertilizer prescription."

prescription_properties[["Micronutrients_possible"]]<-
 "Available research information indicates that possible micronutrient (Cu, Zn, Fe, Mn) deficiencies do not prevent favourable responses to N fertilization. Therefore, the addition of these nutrients to the fertilizer prescription is currently not recommended."

prescription_properties[["Nitrogen_Fertilization_deficient"]]<-
 "Foliar diagnosis indicates N deficiency. Recommended fertilizer prescription to correct N deficiency is 175-225 kg N/ha, using urea (46-0-0) as the primary N source."

prescription_properties[["Nitrogen_Fertilization_none"]]<-
 "Results from research trials in the B.C. interior indicate poor fertilization response potential in stands where pre-fertilization foliar N is > 1.35%. Although other deficiencies may exist, the addition of other nutrients will likely not result in significant growth responses in the absence of N deficiency."

prescription_properties[["N_S_none1"]]<-
 "In the absence of foliar SO4 data, the need to include sulphur in the fertilizer prescription cannot be reliably determined. However, foliar N/S indicates that inclusion of S in the fertilizer prescription is likely not warranted."

prescription_properties[["N_S_none2"]]<-
 "In the absence of foliar SO4 data, the need to include sulphur in the fertilizer prescription cannot be reliably determined. However, foliar N/S indicates that S deficiency may potentially be induced by N fertilization. Results from research trials indicate that growth response may be improved if S is added in combination with N. The inclusion of 50-75 kg S/ha (as ammonium sulphate [21-0-0]) in the fertilizer prescription may be warranted."

prescription_properties[["N_S_none1_other_labs"]]<-
 "In the absence of 'normalized' foliar SO4 data, the need to include sulphur in the fertilizer prescription cannot be reliably determined. However, foliar N/S indicates that S deficiency may potentially be induced by N fertilization. Results from research trials indicate that growth response may be improved if S is added in combination with N. The inclusion of 50-75 kg S/ha (as ammonium sulphate [21-0-0]) in the fertilizer prescription may be warranted."

prescription_properties[["N_S_none2_other_labs"]]<-
 "In the absence of 'normalized' foliar SO4 data, the need to include sulphur in the fertilizer prescription cannot be reliably determined. However, foliar N/S indicates that inclusion of S in the fertilizer prescription is likely not warranted." 

prescription_properties[["N_S_deficient"]]<-
 "Foliar diagnosis indicates S deficiency may be induced or exacerbated by N fertilization. Results from research trials indicate that growth response will likely be improved if S is added in combination with N. The inclusion of 50-75 kg S/ha (as ammonium sulphate [21-0-0]) in the fertilizer prescription may be warranted."

prescription_properties[["Other_Labs_Sulphur"]]<-
 "In the absence of 'normalized' foliar S and SO4 data, the need to include S in the fertilizer prescription cannot be reliably determined."

prescription_properties[["Phosphorus_Deficiency_severe_or_moderate"]]<-
 "Foliar diagnosis indicates P is deficient. Growth response of lodgepole pine following P fertilization in the B.C. interior is not well documented. It is recommended that a tree nutrition specialist be consulted before including P in the fertilizer prescription."

prescription_properties[["Phosphorus_Ratio_Deficiency_severe_or_moderate"]]<-
 "Foliar diagnosis indicates N/P is deficient. Growth response of lodgepole pine following P fertilization in the B.C. interior is not well documented. It is recommended that a tree nutrition specialist be consulted before including P in the fertilizer prescription."

prescription_properties[["Potassium_Deficiency_severe_or_moderate"]]<-
 "Foliar diagnosis indicates K is deficient. Growth response of lodgepole pine following K fertilization in the B.C. interior is not well documented. It is recommended that a tree nutrition specialist be consulted before including K in the fertilizer prescription."

prescription_properties[["Potassium_Ratio_Deficiency_severe_or_moderate"]]<-
 "Foliar diagnosis indicates N/K is deficient. Growth response of lodgepole pine following K fertilization in the B.C. interior is not well documented. It is recommended that a tree nutrition specialist be consulted before including K in the fertilizer prescription."

prescription_properties[["Sulphate_severe"]]<-
 "Foliar diagnosis indicates S deficiency will likely be induced or exacerbated by N fertilization. Results from research trials in B.C. indicate growth response will likely be minimal unless S is added in combination with N. It is recommended that 50-75 kg S/ha (as ammonium sulphate [21-0-0]) be included in the fertilizer prescription."

prescription_properties[["Sulphate_moderate"]]<-
 "Foliar diagnosis indicates S deficiency will likely be induced or exacerbated by N fertilization. Results from research trials in the B.C. interior indicate growth response may be significantly improved if S is added in combination with N. It is recommended that 50-75 kg S/ha (as ammonium sulphate [21-0-0]) be included in the fertilizer prescription."

prescription_properties[["Sulphate_slight"]]<-
 "Foliar diagnosis indicates S deficiency may potentially be induced by N fertilization. Results from research trials indicate that growth response may be improved if S is added in combination with N. The inclusion of 50-75 kg S/ha (as ammonium sulphate [21-0-0]) in the fertilizer prescription may be warranted."

prescription_properties[["Sulphate_none"]]<-
 "Foliar diagnosis indicates that inclusion of S in the fertilizer prescription is likely not warranted."

prescription_properties[["Zinc_Deficiency_probable"]]<-
 "Foliar diagnosis indicates Zn deficiency is probable. Growth response of lodgepole pine following Zn fertilization in the B.C. interior is not well documented. It is recommended that a tree nutrition specialist be consulted before including Zn in the fertilizer prescription."






#-------------------------------------------------------------------------------------------------------------------------------------------------------
# This is the function for writing Comments
#-------------------------------------------------------------------------------------------------------------------------------------------------------



site_properties <- list()

site_properties[["Boron_probable"]]<-
 
 "Foliar B concentration indicates a probable B deficiency. Deficiency symptoms may, or may not, be visible. In lodgepole pine, the most common and noticeable visual symptoms of B deficiency are shoot, shoot tip, or bud dieback of the leader and/or lateral branches resulting in multi-leadered, bush crowns. Needles near the affected tips are often short and deformed, either dark green, or discolored. Needle malformations often occur before other external symptoms. It is suggested that the stand be re-visited to confirm whether visual symptoms of B deficiency exist."

site_properties[["Boron_severe"]]<-
 
 "Foliar B concentration indicates a severe B deficiency. Deficiency symptoms are likely visible, or will become visible after N fertilization. In lodgepole pine, the most common and noticeable visual symptoms of B deficiency are shoot, shoot tip, or bud dieback of the leader and/or lateral bracnches resulting in multi-leadered, bush crowns. Needles near the affected tips are often short and deformed, either dark green, or discolored. Needle malformations often occur before other external symptoms. It is suggested that the stand be re-visited to confirm whether visual symptoms of B deficiency exist."

site_properties[["Composite_Number_Comp_Number"]]<-
 
 "Because there may be considerable inter-tree variation in foliar nutrient levels, a composite sample consisting of foliage from less than 10 trees per stand (or stratum) may not provide reliable estimates of mean foliar concentrations for many nutrients."

site_properties[["Diagnosis_is_based_on_ind_sample"]]<-
 
 "Because there may be considerable inter-tree variation in foliar nutrient levels, foliage from 15 to 20 trees per stand (or stratum) is needed to provide reliable estimates of mean foliar concentrations for most nutrients. For routine diagnostic use, a composite sample, prepared by combining foliage collected from 15 to 20 trees per stand (or stratum), is a cost-effective way of assessing stand foliar nutrient status. Refer to <A HREF='http://www.for.gov.bc.ca/hfd/pubs/Docs/En/En52.pdf'>'Foliar sampling guidelines and nutrient interpretative criteria for lodgepole pine'</A> for further information."

site_properties[["Laboratory_Laboratory_Output"]]<-
 
 "Foliar analytical results for some nutrients differ depending on the methodology used for extraction and determination. In some cases, differences may be large enough to affect diagnoses of nutrient sufficiency or deficiency based on available interpretative criteria. Using functions developed from inter-laboratory comparisons, the submitted foliar values for some nutrients have been adjusted to 'normalized' values from which diagnoses and fertilizer prescriptions have been developed. Normalization is necessary for consistent application of diagnostic and prescriptive rules. <I><b>No inferences should be drawn from the normalization requirement regarding the quality or integrity of the submitted foliar data.</b></I> Submitted and 'normalized' foliar nutrient values (and calculated nutrient ratios), are both shown in the above tabular output."

site_properties[["Laboratory_Other_Labs"]]<-
 
 "Foliar analytical results for some nutrients differ depending on the methodology used for extraction and determination. In some cases, differences may be large enough to affect diagnoses of nutrient sufficiency or deficiency based on available interpretative criteria. For some laboratories, inter-laboratory comparisons have been undertaken to develop functions that can be used to adjust foliar levels to 'normalized' values from which interpretations and fertilizer prescriptions are then made. Because functions have not yet been developed for the laboratory to which this sample was submitted, output should be reviewed with caution, especially with respect to S diagnosis and prescription.<I><b> However, no inferences should be drawn regarding the quality or integrity of the submitted foliar data.</b></I> "

site_properties[["Percent_Live_Crown_Crown"]]<-
 
 "The percent live crown of trees may be too small to efficiently utilize added nutrients following fertilization and, as such, growth response potential may be relatively low."

site_properties[["Pertinent_site_stand_information_stem_cankers"]]<-
 
 "The presence of stem cankers or rusts may prevent reliable diagnosis of stand nutrient status if foliage was collected from affected trees. Refer to <A HREF='http://www.for.gov.bc.ca/hfd/pubs/Docs/En/En52.pdf">"Foliar sampling guidelines and nutrient interpretative criteria for lodgepole pine'</A> for further information."

site_properties[["Pertinent_site_stand_information_small_damage"]]<-
 
 "The presence of feeding injuries (i.e., stem de-barking) by red squirrels or other animals may prevent reliable diagnosis of stand nutrient status if foliage was collected from damaged trees. Sharp increases in the abundance and severity of feeding injuries have been documented following fertilization of lodgepole pine in the interior of British Columbia. It is recommended that a forest health specialist be consulted if fertilization of this stand is contemplated. "

site_properties[["Pertinent_site_stand_information_poorly_drained"]]<-
 
 "Foliar nutrient status may be affected by excessively wet or dry soil conditions that influence the transport and availability of soil nutrients and the activity and vigour of tree roots. Because soil moisture may exert the primary limitation on tree growth, fertilization of excessively wet or dry sites is not recommended."

site_properties[["Pertinent_site_stand_information_severe_needle"]]<-
 
 "The presence of needle casts (e.g., <I>Lophodermella concolor</I>) or blights may prevent reliable diagnosis of stand nutrient status. Dead and discoloured foliage may have lower nutrient levels than unaffected foliage. Nutrient levels in green, current-year''s foliage may be affected by the infection and defoliation of older foliage. "

site_properties[["Ratio_N_P_Deficiency_Slight"]]<-
 
 "Phosphorus deficiency may be induced by nitrogen fertilization. "

site_properties[["Ratio_N_K_Deficiency_Slight"]]<-
 
 "Potassium deficiency may be induced by nitrogen fertilization. "

site_properties[["Foliage_Age_Not_Current_Year"]]<-
 
 "Foliage other than current year''s foliage was submitted for analysis and, as such, may prevent reliable diagnosis of stand nutrient status. Foliar nutrient concentrations may be strongly affected by the age of collected foliage. Foliar interpretative criteria are based on analysis of current year''s foliage. Refer to <A HREF='http://www.for.gov.bc.ca/hfd/pubs/Docs/En/En52.pdf'>'Foliar sampling guidelines and nutrient interpretative criteria for lodgepole pine'</A> for further information. "

site_properties[["Sampling_Month_incorrect"]]<-
 
 "Foliage was collected at a time of year not normally recommended for foliage sampling and, as such, may prevent reliable diagnosis of stand nutrient status. Foliar nutrient concentrations may be strongly affected by the time of year in which foliage is collected. Foliar interpretative criteria are based on analysis of foliage collected during the dormant season. Refer to <A HREF='http://www.for.gov.bc.ca/hfd/pubs/Docs/En/En52.pdf'>'Foliar sampling guidelines and nutrient interpretative criteria for lodgepole pine'</A> for further information. "

site_properties[["Site_Information_Poorly_Drained"]]<-
 
 "Poorly drained soils may affect soil nutrient availability and/or nutrient uptake by tree roots. As such foliar nutrient levels may not accurately reflect soil nutrient status."

site_properties[["Visual_Deficiency_Symptoms_top_dieback_none"]]<-
 
 "Top dieback is a common visual symptom of B deficiency in lodgepole pine. Because foliar B levels in this stand suggest B is not deficient, the observed top dieback symptoms are likely not nutritionally related."

site_properties[["Visual_Deficiency_Symptoms_top_dieback_notlikely"]]<-
 
 "A boron deficiency, causing reduced height increment and/or top die-back, may be induced by nitrogen fertilization"

site_properties[["Visual_Deficiency_Symptoms_top_dieback_possible"]]<-
 
 "A boron deficiency, causing reduced height increment and/or top die-back, may be induced by nitrogen fertilization"

site_properties[["Visual_Deficiency_Symptoms_top_dieback_likely"]]<-
 
 "Sub-acute B deficiency, causing reduced height increment, likely exists in the absence of visual deficiency symptoms (i.e., top die-back)."

site_properties[["Visual_Deficiency_Symptoms_top_dieback_severe"]]<-
 
 "Visual symptoms of B deficiency (i.e., top die-back) likely present. "

site_properties[["Visual_Defficiency_Severe_Needle_Cast"]]<-
 
 "Needle casts (e.g., <I>Lophodermella concolor</I>) and blights may affect foliar nutrient levels. Discolored foliage may have lower nutrient levels than unaffected foliage. Also, nutrient levels in green foliage sampled from trees with severe defoliation may not accurately reflect stand nutrient status."

site_properties[["Crown_Position_Middle_Lower"]]<-
 
 "Foliage was collected from a crown position not normally recommended for foliage sampling and, as such, may prevent reliable diagnosis of stand nutrient status. Foliar nutrient concentrations may be strongly affected by crown position. Foliar interpretative criteria are based on foliage collected from between the top one-quarter and bottom one-half of the live crown. Refer to <A HREF='http://www.for.gov.bc.ca/hfd/pubs/Docs/En/En52.pdf'>'Foliar sampling guidelines and nutrient interpretative criteria for lodgepole pine'</A> for further information."
