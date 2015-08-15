%% mydata_my_pet
% Sets referenced data

%%
function [data, txt_data, metadata] = mydata_Pseudemydura_umbrina 
  % created by Starrlight Augustine, Bas Kooijman, Dina Lika, Goncalo Marques and Laure Pecquerie 2015/03/31
  
  %% Syntax
  % [data, txt_data, metadata] = <../mydata_my_pet.m *mydata_my_pet*>
  
  %% Description
  % Sets data, pseudodata, metadata, explanatory text, weight coefficients.
  % Meant to be a template in add_my_pet
  %
  % Output
  %
  % * data: structure with data
  % * txt_data: text vector for the presentation of results
  % * metadata: structure with info about this entry
  
  %% To do (remove these remarks after editing this file)
  % * copy this template; replace 'my_pet' by the name of your species
  % * fill in metadata fields with the proper information
  % * insert references for the data (an example is given)
  % * edit fact-list for your species, where you can add species and/or data properties
  % * edit real data; remove all data that to not belong to your pet
  % * complete reference list
  % * OPTIONAL : add discussion points / comments before the reference list

%% set metadata

T_C = 273.15; % K, temperature at 0 degrees C (used in T_typical)

metadata.phylum     = 'Chordata'; 
metadata.class      = 'Reptilia'; 
metadata.order      = 'Testudines'; 
metadata.family     = 'Chelidae';
metadata.species    = 'Pseudemydura_umbrina'; 
metadata.species_en = 'Western Swamp Turtle';
metadata.T_typical  = T_C + 17.7; % K
metadata.data_0     = {'ab'; 'ap'; 'am'; 'Lb'; 'Lp'; 'Li'; 'Wdb'; 'Wdp'; 'Wdi'; 'Ri'};  % tags for different types of zero-variate data
%metadata.data_1     = {'t-L', 'L-W', 'T_O'}; % tags for different types of uni-variate data
metadata.data_1     = {'T_O','t-L', 'L-W', 't-W'}; % tags for different types of uni-variate data

metadata.COMPLETE = 3.3; % Using criteria of LikaKear2011

metadata.author   = {'Sophie Arnall'};                       
metadata.date_acc = [2013 05 03];                         
metadata.email    = {'arnals01@student.uwa.edu.au'};
metadata.address  = {'University of Western Australia, 6009, Australia'}; 

% uncomment and fill in the following fields when the entry is updated:
% metadata.author_mod_1  = {'author2'};                       % put names as authors as separate strings:  {'author1','author2'} , with corresponding author in first place 
% metadata.date_mod_1    = [2017 09 18];                      % [year month day], date modified entry is accepted into the collection
% metadata.email_mod_1   = {'myname@myuniv.univ'};            % e-mail of corresponding author
% metadata.address_mod_1 = {'affiliation, zipcode, country'}; % affiliation, postcode, country of the corresponding author

%% set data
% zero-variate data;
% typically depend on scaled functional response f.
% here assumed to be equal for all real data; the value of f is specified in pars_init_my_pet.

% age 0 is at onset of embryo development
data.ab = 250;      units.ab = 'd';    label.ab = 'age at birth';                bibkey.ab = 'Mitchell_et_al_2006';
  temp.ab = T_C + 24.532;  bibkey.ab = 'Mitchell_et_al_2006'; % K, temperature, based on ;
  % observed age at birth is frequently larger than ab, because of diapauzes during incubation
data.ap = data.ab+11*365;     units.ap = 'd';    label.ap = 'age at puberty';              bibkey.ap = 'Jarvie_unpub';
  temp.ap = T_C + 21;  bibkey.ap = 'Jarvie_unpub'; % K, temperature, based on simulation of Tb from 2000-2013 at Orford, see last lines of Sphenodon_punctatus.R;;
  % observed age at puberty is frequently larger than ap, 
  %   because allocation to reproduction starts before first eggs appear
data.am = 100*365;     units.am = 'd';    label.am = 'life span';                   bibkey.am = 'Dawbin_1982';
  temp.am = T_C + 21;  bibkey.am = 'Jarvie_unpub'; % K, temperature, based on simulation of Tb from 2000-2013 at Stephens Island/Takapourewa, see last lines of Sphenodon_punctatus.R;;
% (accounting for aging only) 

% Please specify what type of length measurement is used for your species.
% We put here snout-to-vent length, but you should change this depending on your species:
data.Lb  = 2.6;   units.Lb  = 'cm';   label.Lb  = 'snout to vent length at birth';    bibkey.Lb  = 'Cree_unpub';
data.Lp  = 10.7;   units.Lp  = 'cm';   label.Lp  = 'snout to vent length at puberty';  bibkey.Lp  = 'Cree_1994';
data.Li  = 13.1;   units.Li  = 'cm';   label.Li  = 'ultimate snout to vent length';    bibkey.Li  = 'Jarvie_unpub';
data.Wdb = 5.2*0.3; units.Wdb = 'g';    label.Wdb = 'dry weight at birth';              bibkey.Wdb = 'Cree_unpub';
%find dry weight at puberty
data.Wdp = 236.5*0.3;   units.Wdp = 'g';    label.Wdp = 'dry weight at puberty';            bibkey.Wdp = 'Cree_1994';
data.Wdi = 417*0.3;   units.Wdi = 'g';    label.Wdi = 'ultimate dry weight';              bibkey.Wdi = 'Dawbin_1982';
data.Ri  = 5/365;    units.Ri  = '#/d';  label.Ri  = 'maximum reprod rate';              bibkey.Ri  = 'Cree_1994';
  % for an individual of ultimate length Li 
  temp.Ri = T_C +  15.52;  bibkey.Ri = 'Jarvie_unpub'; % K, temperature, based on simulation of Tb from 2000-2013 at Stephens Island/Takapourewa, see last lines of Sphenodon_punctatus traits.R;
 
% uni-variate data

% uni-variate data at f = 1.0 (this value should be added in pars_init_my_pet as a parameter f_tL) 
% snout-to-vent length and wet weight were measured at the same time
%data.tL = [0	539.105	1036.6	1526.43	2023.925	2573.615	3170.39	3872.285	4534.395	5254.905	5982.715	6844.115	7784.355	8729.705	9691.115	10644.13	11670.875	12692.145	13765.975	14779.58	15753.765	16809.345	17830.98	18897.145	19950.17	20934.94	21851.09;    % d, time since birth
%           5.037	6.256	7.4811	8.725	9.8895	11.0948	12.2177	13.3859	14.4665	15.4192	16.2948	17.0951	17.8422	18.5323	19.1243	19.6361	20.0568	20.3866	20.6559	20.9325	21.1449	21.3802	21.5316	21.6226	21.7174	21.7815	21.8688]';  % cm, snout-to-vent length at f and T
%units.tL = {'d', 'cm'};     label.tL = {'time since birth', 'snout to vent length'};  bibkey.tL = 'Dawbin_1982';
%  temp.tL = T_C + 15.52;  % K, temperature

data.tL = [1	8	15	22	29	36	43	50	57	64	71	78	84	91	98	105	112;    % d, time since birth
           2.36	2.52	2.23	2.7	2.82	2.91	3.08	3.22	3.33	3.47	3.72	3.87	3.98	4.12	4.18	4.29    4.3]';  % cm, snout-to-vent length at f and T
units.tL = {'d', 'cm'};     label.tL = {'time since birth', 'snout to vent length'};  bibkey.tL = 'Dawbin_1982';
  temp.tL = T_C + 17.5;  % K, temperature

data.LW = [2.36	2.52	2.23	2.7	2.82	2.91	3.08	3.22	3.33	3.47	3.72	3.87	3.98	4.12	4.18	4.29    4.3;      % cm, snout-to-vent length at f
           4.1	4.7	5	5.7	6.3	6.9	7.4	8.2	8.9	9.6	11.7	13.1	13.9	14.7	15.6	16.5	17.5]';   % g, wet weight at f and T
units.LW = {'cm', 'g'};     label.LW = {'snout to vent length', 'wet weight'};  bibkey.LW = 'Dawbin_1982';
data.tW = [ ...
1	5.7
280	40.5
369	34.9
386	43
614	59.9
736	53.5
766	59.8
809	61.4
838	61.3
868	61.7
898	63.1
930	67.4
950	64.1
1129	71
1150	72.9
1180	74.5
1206	73.9
1235	74.5
1262	83.2
1290	95.8
1317	99.5
1340	93.7
1468	101.3
1508	101.9
1542	100.4
1570	103.1
1605	104.1
1627	104.7
1659	103.9
1699	98
1844	102.5
1879	104.9
1910	107.6
1940	114.3
1972	118.1
2001	131.5
2032	137.1
2059	137.2
2078	133
2204	137
2266	137
2295	139.1
2326	141.1
2360	146.6
2394	143
2441	135
2583	142.7
2625	145.9
2657	149.1
2688	152.9
2716	164.7
2745	175.3
2778	182.7
2805	163.2
2850	184
2880	182
2934	173.8
3008	177.4
3037	183
3066	187.5
3094	195.5
3121	188.1
3154	177.5
3315	189.1
3378	189.2
3391	192.5
3439	198.1
3470	194.9
3538	183.5
3692	184.3
3725	189.9
3754	195.6
3784	205.1
3819	217.3
3862	223.3
3883	237.8
3915	241.1
3946	233.4
3969	226
4049	215
4074	226.6
4084	225.5
4106	229.2
4121	230.9
4140	234.4
4147	238.4
4176	249.2
4200	233.4
4232	235.1
4266	225.6
4416	219.2
4450	222.6
4452	210.8
4480	215.8
4481	207
4500	233.2
4533	253.4
4570	246.9
4599	244.9
4632	227.2
4779	237
4810	250
4867	245.9
4937	257.1
4986	259.2
5028	260
5059	257.9
5088	245.5
5147	258.3
5176	257
5196	262.3
5213	267.7
5231	261.9
5232	268.9
5247	280.5
5254	282.2
5265	284.6
5272	288.7
5283	287.6
5293	261.2
5299	271.2
5311	280.4
5316	278.9
5329	254.9
5360	237.8
5502	251.4
5536	253.4
5552	248.5
5554	262.4
5609	273.8
5621	273
5631	280.3
5642	289.7
5651	271.1
5704	263.9
5725	235.3];
units.tW = {'days', 'g'};     label.tW = {'age', 'mass'};  bibkey.tW = '';

%  data.TO = [ ... temp (C), O2 consumption (ml/h.gwet) of 18.90108 g wet weight (5.67 g dry), based on Scott's data
% 5   0.016422
% 12	0.002366757/12*60
% 20	0.007695684/12*60
% 24	0.011011122/12*60
% 27	0.020467552/12*60
% 29	0.028250338/12*60
% 30	0.032857636/12*60
% ];
%units.TO = {'mlO2/gwet/h', 'C'};     label.TO = {'O2 consumption rate', 'temperature'};  bibkey.TO = 'Scott_unpub; Cartland_Grimmond_1994_5C_medium';
%ID 139; wet weight 362.4g
data.TO = [ ... 
%data.TO_139 = [ ... 
15	0.002085919
21	0.006957514
25	0.01929228
31	0.03080439%];
%units.TO_139 = {'C', 'ml O2/g/h'};     label.TO_139 = {'temp', 'O2 consumption'};  bibkey.TO_139 = 'Arnall et al. 2015';

% ID 204; wet weight 288.2g
%data.TO_204 = [ ... 
15	0.002556972
21	0.007311692
25	0.01694881
31	0.02668169%];
%units.TO_204 = {'C', 'ml O2/g/h'};     label.TO_204 = {'temp', 'O2 consumption'};  bibkey.TO_204 = 'Arnall et al. 2015';

% ID 269; wet weight 297.2g
%data.TO_269 = [ ... 
15	0.001478324
21	0.005955151
25	0.01790451
31	0.03496208%];
%units.TO_269 = {'C', 'ml O2/g/h'};     label.TO_269 = {'temp', 'O2 consumption'};  bibkey.TO_269 = 'Arnall et al. 2015';


% ID 354; wet weight 277.3g
%data.TO_354 = [ ... 
15	0.001204873
21	0.006027985
25	0.01742385
31	0.02480775%];
%units.TO_354 = {'C', 'ml O2/g/h'};     label.TO_354 = {'temp', 'O2 consumption'};  bibkey.TO_354 = 'Arnall et al. 2015';


% ID 524; wet weight 322.0g
%data.TO_524 = [ ... 
15	0.001312231
21	0.005149532
25	0.01897694
31	0.02305341%];
%units.TO_524 = {'C', 'ml O2/g/h'};     label.TO_524 = {'temp', 'O2 consumption'};  bibkey.TO_524 = 'Arnall et al. 2015';


% ID 690; wet weight 293.5g
%data.TO_690 = [ ... 
15	0.002410638
21	0.008921721
25	0.01756489
31	0.02747758%];
%units.TO_690 = {'C', 'ml O2/g/h'};     label.TO_690 = {'temp', 'O2 consumption'};  bibkey.TO_690 = 'Arnall et al. 2015';


% ID 716; wet weight 269.3g
%data.TO_716 = [ ... 
15	0.001236561
21	0.006155903
25	0.01767426
31	0.02939617%];
%units.TO_716 = {'C', 'ml O2/g/h'};     label.TO_716 = {'temp', 'O2 consumption'};  bibkey.TO_716 = 'Arnall et al. 2015';


% ID Z1; wet weight 308.7g
%data.TO_Z1 = [ ... 
15	0.002687078
21	0.005811912
25	0.01850031
31	0.03345558];
units.TO_Z1 = {'C', 'ml O2/g/h'};     label.TO_Z1 = {'temp', 'O2 consumption'};  bibkey.TO_Z1 = 'Arnall et al. 2015';
units.TO = {'C', 'ml O2/g/h'};     label.TO = {'temp', 'O2 consumption'};  bibkey.TO = 'Arnall et al. 2015';

%% set weights for all real data
weight = setweights(data, []);

%% overwriting weights (remove these remarks after editing the file)
% the weights were set automatically with the function setweigths,
% if one wants to ovewrite one of the weights it should always present an explanation example:
%
% zero-variate data:
% weight.Wdi = 100 * weight.Wdi; % Much more confidence in the ultimate dry
%                                % weight than the other data points
 %weight.Ri = 50*weight.Ri;
 weight.Wdb = 20*weight.Wdb;
%weight.Wdp = 10*weight.Wdp;
 weight.Wdi = 20*weight.Wdi;
 %weight.ap = 50*weight.ap;
 weight.ab = 20*weight.ab;
 weight.Li = 20*weight.Li;
 weight.Li = 20*weight.Lb;

% uni-variate data: 
% weight.TO = 50 * weight.TO;
 weight.LW = 100 * weight.LW;
weight.tL = 100 * weight.tL;
weight.tW = 100 * weight.tW;
%% set pseudodata and respective weights
% (pseudo data are in data.psd and weights are in weight.psd)
[data, units, label, weight] = addpseudodata(data, units, label, weight);

%% overwriting pseudodata and respective weights (remove these remarks after editing the file)
% the pseudodata and respective weights were set automatically with the function setpseudodata
% if one wants to ovewrite one of the values it should always present an explanation
% example:
% data.psd.p_M = 1000;                    % my_pet belongs to a group with high somatic maint 
% weight.psd.kap = 10 * weight.psd.kap;   % I need to give this pseudo data a higher weight

%% pack data and txt_data for output
data.weight = weight;
data.temp = temp;
txt_data.units = units;
txt_data.label = label;
txt_data.bibkey = bibkey;

%% References
  bibkey = 'Wiki'; type = 'Misc'; bib = ...
  'URL = {https://en.wikipedia.org/wiki/Tuatara}';   % replace my_pet by latin species name
  eval(['metadata.biblist.' bibkey, '= ''@', type, '{', bibkey, ', ' bib, '}'';']);
  %
  bibkey = 'Kooy2010'; type = 'Book'; bib = [ ...  % used in setting of chemical parameters and pseudodata
  'author = {Kooijman, S.A.L.M.}, ' ...
  'year = {2010}, ' ...
  'title  = {Dynamic Energy Budget theory for metabolic organisation}, ' ...
  'publisher = {Cambridge Univ. Press, Cambridge}, ' ...
  'pages = {Table 4.2 (page 150), 8.1 (page 300)}, ' ...
  'URL = {http://www.bio.vu.nl/thb/research/bib/Kooy2010.html}'];
  eval(['metadata.biblist.' bibkey, '= ''@', type, '{', bibkey, ', ' bib, '}'';']);
  %
  bibkey = 'Jarvie_unpub'; type = 'Thesis'; bib = [ ... % meant as example; replace this and further bib entries
  'author = {Jarvie, S. and Cree, A.}, ' ...
  'year = {2015}, ' ...
  'title = {TBA}'];
  eval(['metadata.biblist.' bibkey, '= ''@', type, '{', bibkey, ', ' bib, '}'';']);
  %
  bibkey = 'Anon2015'; type = 'Misc'; bib = [ ...
  'author = {Anonymous}, ' ...
  'year = {2015}, ' ...
  'URL = {http://www.fishbase.org/summary/Rhincodon-typus.html}'];
  eval(['metadata.biblist.' bibkey, '= ''@', type, '{', bibkey, ', ' bib, '}'';']);

%% Facts
% * Standard model with egg (not foetal) development and no acceleration
  
%% Discussion points
pt1 = 'Kearney: there is a github repository for this project git/mrke/tuatara';
pt2 = 'Kearney: TA was estimated from Yuni''s unpublished data on sprint speed (/sprint speed/sprint_speed_N_occelatus_Yuni.csv), using script /sprint speed/TA from sprint speed.R';
pt3 = 'Jarvie: metabolic rates were extracted from Jarvie''s measurements of metabolic rate at six temperatures (12, 20, 24, 27, 29 and 30C). We only used metabolic rate for animals presumed to be females, due to the temperatures that they were incubated at. We also used a metabolic rate measurement at 5C, for medium-sized animals, from Cartland Grimmond 1994';
pt4 = 'Kearney: Temperatures for ';     
metadata.discussion = {pt1; pt2; pt3; pt4};
