function [data, txt_data, metadata] = mydata_Pseudemydura_umbrina 
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
metadata.data_1     = {'t_L'; 't_W'; 'L_W'; 'T_O'}; % tags for different types of uni-variate data

metadata.COMPLETE = 3.3; % Using criteria of LikaKear2011

metadata.author   = {'Sophie Arnall'};                       
metadata.date_acc = [2016 02 12];                         
metadata.email    = {'sophie.arnall@research.uwa.edu.au'};
metadata.address  = {'University of Western Australia, 6009, Australia'}; 

%% set data
% zero-variate data (taken from literature and zoo females)
data.ab = 153;      units.ab = 'd';     label.ab = 'age at birth';                  bibkey.ab = 'S. Arnall unpublished data';                                comment.ab = 'Average of 8 individuals born in captivity that were incubated at 24oC constant for entire incubation period (range 126-172 days; born prior to May 1; IDs 942, 945, 946, 991, 997, 1086, 1087, 1088)';
  temp.ab = T_C + 24.532;                                                                                                                                    comment.temp.ab = 'K, temperature; actual temp in incubator (confirmed constant; retrospective correction incubators 1,2 and 4)';
                                                                                                                        
data.ap = 3767;     units.ap = 'd';     label.ap = 'age at puberty';                bibkey.ap = 'S. Arnall unpublished data';                                comment.ap = 'Average time for the 12 adult females born in captivity to have first become gravid (scanned gravid; IDs 261, 324, 378, 468, 490, 496, 501, 504, 525, 618, 635, 728)';
  temp.ap = T_C + 21;                                                                                                                                        comment.temp.ab = 'K, temperature; approximate CTE';
                                                                                                                       
data.am = 36500;    units.am = 'd';     label.am = 'life span';                     bibkey.am = 'S. Arnall unpublished data';                                comment.am = 'UNKNOWN. Best guess: certainly greater than 61 years (females acquired as adults in 1963 are still producing viable clutches), likely 80-100 (G. Kuchling pers comm.)';   
  temp.am = T_C + 21;        

data.Lb  = 2.6;      units.Lb  = 'cm';   label.Lb  = 'carapace length at birth';      bibkey.Lb  = 'S. Arnall unpublished data';                                comment.Lb  = 'Average of ab individuals';
data.Lp  = 10.7;     units.Lp  = 'cm';   label.Lp  = 'carapace length at puberty';    bibkey.Lp  = 'S. Arnall unpublished data';                                comment.Lp = 'Average of ap individuals, taken at first instance of being gravid';
data.Li  = 13.1;     units.Li  = 'cm';   label.Li  = 'ultimate carapace length';      bibkey.Li  = {'S. Arnall unpublished data'; 'Burbidge et al. 2010'};      comment.Li = 'Best guess, based on CZ2 (largest female) who has grown less than 3mm in 10 years; "Females do not grow beyond 135mm carapace length" (Burbidge et al. 2010)';
data.Wdb = 5.2;      units.Wdb = 'g';    label.Wdb = 'wet weight at birth';           bibkey.Wdb = {'S. Arnall unpublished data'; 'Burbidge et al. 2010'};      comment.Wdb  = 'Average of ab individuals';
data.Wdp = 236.5;    units.Wdp = 'g';    label.Wdp = 'wet weight at puberty';         bibkey.Wdp = 'S. Arnall unpublished data';                                comment.Wdp = 'Average of ap individuals, taken at first instance of being gravid';
data.Wdi = 417;      units.Wdi = 'g';    label.Wdi = 'ultimate wet weight';           bibkey.Wdi = 'S. Arnall unpublished data';                                comment.Wdi = 'Best guess, based on maximum weight recorded for CZ2 (largest female; 417.1g); "Females do not exceed 410g" (Burbidge et al. 2010); revision from 408g in Burbidge 1981';
data.Ri  = 5/365;    units.Ri  = '#/d';  label.Ri  = 'maximum reprod rate';           bibkey.Ri  = 'Burbidge 1981';                                             comment.Ri = 'Females lay 3-5 eggs';  
temp.Ri = T_C + 21; 

%% uni-variate data
% carapace length and wet weight were measured at the same time
%==========================================================================
%TIME-LENGTH CAPTIVE FEMALE SET; n=6
data.tL = [ ...
% ID 261
1       2.56
382     5.38
740     6.82
1117	6.91
1505	7.86
1608	7.84
1886	7.93
2276	8.45
2646	8.46
3027	9.42
3369	10.33
3793	10.46
4145	10.57
4838	10.95
5223	10.98
5567	11.07
6309	11.21
% ID 378
1       2.59
363     6.53
717     6.88
1135	6.91
1511	7.35
1888	8.38
2241	8.88
2611	10.21
3008	10.27
3430	10.27
4081	10.34
4469	10.42
4810	10.69
5556	11.11
% ID 501
1       2.81
365     7.03
794     7.8
1152	7.93
1524	8.5
1911	9.77
2277	10.35
2994	10.75
3386	10.76
3722	10.96
4477	11.16
% ID 525
1       2.79
400     6.88
794     8.04
1160	8.34
1559	9.09
1918	9.79
2671	10.62
3018	10.78
3367	10.96
% ID 635
1       2.58
407     6.87
765     7.61
1165	8.05
1921	8.26
2268	8.67
2615	9.15
2977	9.9
3345	9.95
% ID 728
1       2.5
420     7.18
1186	8.86
1533	9.17
1881	9.35
2242	9.71
2610	10.34];
units.tL = {'d', 'mm'}; label.tL = {'time since birth', 'midline carapace length'};  bibkey.tL = ''; temp.tL = T_C + 21;  % K,

%==========================================================================
% LENGTH-WEIGHT CAPTIVE FEMALE SET; n=6
data.LW = [ ...
% ID 261
2.56	4.7
5.38	32.7
6.82	62.4
6.91	64.5
7.86	91.4
7.84	94.3
7.93	95.7
8.45	118.7
8.46	119
9.42	150.7
10.33	195.4
10.46	212.3
10.57	218.5
10.95	229.2
10.98	228.8
11.07	232.1
11.21	247.2
% ID 378
2.59	4.3
6.53	51.2
6.88	62.3
6.91	64.7
7.35	77.5
8.38	98.3
8.88	117.7
10.21	176.7
10.27	185.3
10.27	190.3
10.34	191.7
10.42	196.3
10.69	203.6
11.11	230.9
% ID 501
2.81	5.5
7.03	67.5
7.8     82.7
7.93	92
8.5     105.7
9.77	152.9
10.35	183
10.75	193
10.76	198.8
10.96	203.2
11.16	221.3
% ID 525
2.79	5.6
6.88	57.8
8.04	94.6
8.34	102.7
9.09	128
9.79	155.5
10.62	207.3
10.78	213.6
10.96	215
% ID 635
2.58	5.3
6.87	60.9
7.61	82.1
8.05	98
8.26	112.9
8.67	117.9
9.15	142.1
9.9     176
9.95	197.5
% ID 728
2.5     4.8
7.18	68.6
8.86	120.9
9.17	128.1
9.35	135.4
9.71	148.7
10.34	182.7];   
units.LW = {'cm', 'g'}; label.LW = {'midline carapace length', 'wet weight'};  bibkey.LW = ''; 

%==========================================================================
%TIME-WEIGHT CAPTIVE FEMALE SET; n=6
data.tW = [ ...
% ID 261
1       4.7
382     32.7
740     62.4
1117	64.5
1505	91.4
1608	94.3
1886	95.7
2276	118.7
2646	119
3027	150.7
3369	195.4
3793	212.3
4145	218.5
4838	229.2
5223	228.8
5567	232.1
6309	247.2
% ID 378
1       4.3
363     51.2
717     62.3
1135	64.7
1511	77.5
1888	98.3
2241	117.7
2611	176.7
3008	185.3
3430	190.3
4081	191.7
4469	196.3
4810	203.6
5556	230.9
% ID 501
1       5.5
365     67.5
794     82.7
1152	92
1524	105.7
1911	152.9
2277	183
2994	193
3386	198.8
3722	203.2
4477	221.3
% ID 525
1       5.6
400     57.8
794     94.6
1160	102.7
1559	128
1918	155.5
2671	207.3
3018	213.6
3367	215
% ID 635
1       5.3
407     60.9
765     82.1
1165	98
1921	112.9
2268	117.9
2615	142.1
2977	176
3345	197.5
% ID 728
1       4.8
420     68.6
1186	120.9
1533	128.1
1881	135.4
2242	148.7
2610	182.7];
units.tW = {'d', 'g'}; label.tW = {'time since birth', 'wet weight'};  bibkey.tW = ''; temp.tW = T_C + 21;  % K, temperature

%==========================================================================  
% METABOLIC DATA; n=8
%ID 139; wet weight 362.4g
data.TO = [ ... 
15	0.002085919
21	0.006957514
25	0.01929228
31	0.03080439
% ID 204; wet weight 288.2g
15	0.002556972
21	0.007311692
25	0.01694881
31	0.02668169
% ID 269; wet weight 297.2g
15	0.001478324
21	0.005955151
25	0.01790451
31	0.03496208
% ID 354; wet weight 277.3g
15	0.001204873
21	0.006027985
25	0.01742385
31	0.02480775
% ID 524; wet weight 322.0g
15	0.001312231
21	0.005149532
25	0.01897694
31	0.02305341
% ID 690; wet weight 293.5g
15	0.002410638
21	0.008921721
25	0.01756489
31	0.02747758
% ID 716; wet weight 269.3g
15	0.001236561
21	0.006155903
25	0.01767426
31	0.02939617
% ID Z1; wet weight 308.7g
15	0.002687078
21	0.005811912
25	0.01850031
31	0.03345558];
units.TO = {'C', 'ml O2/g/h'}; label.TO = {'temp', 'O2 consumption'};  bibkey.TO = 'Arnall et al. 2015';

%% set weights for all real data
weight = setweights(data, []);

%% overwriting weights (remove these remarks after editing the file)
 weight.Wdb = 20*weight.Wdb;
 weight.Wdi = 20*weight.Wdi;
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
%   %  
%   bibkey = 'S. Arnall unpublished data.'; type = 'Misc'; bib = ...
%   'Arnall, Sophie <arnals01@student.uwa.edu.au>'; 
%   eval(['metadata.biblist.' bibkey, '= ''@', type, '{', bibkey, ', ' bib, '}'';']);
%  
%     %
%   bibkey = 'Burbidge et al. 2010'; type = 'Report'; bib = [ ... 
%   'author = {Burbidge A. A.}, {Kuchling G.}, {Olejnik C.}, {Mutter L.} ' ...
%   'year = {2010}, ' ...
%   'title  = {Western Swamp Tortoise (Pseudemydura umbrina) recovery plan, 4th edition}, ' ...
%   'publisher = {Department of Parks and Wildlife, Western Australia}, ' ...
%   ];
%   eval(['metadata.biblist.' bibkey, '= ''@', type, '{', bibkey, ', ' bib, '}'';']);
%   
%     %
%   bibkey = 'Burbidge 1981'; type = 'Article'; bib = [ ... 
%   'author = {Burbidge A. A.}, ' ... 
%   'year = {1981}, ' ...
%   'title = {The ecology of the western swamp tortoise Pseudemydura umbrina (Testudines: Chelidae)}, ' ...
%   'journal = {Australian Wildlife Research}, ' ...
%   'volume = {8}, ' ...
%   'pages = {203-223}'];
%   eval(['metadata.biblist.' bibkey, '= ''@', type, '{', bibkey, ', ' bib, '}'';']);
%   
%      %
%   bibkey = 'Arnall et al. 2015'; type = 'Article'; bib = [ ... 
%   'author = {Arnall S}, {Kuchling G.}, {Mitchell N.}' ... 
%   'year = {2015}, ' ...
%   'title = {A thermal profile of metabolic performance in the rare Australian chelid, Pseudemydura umbrina}, ' ...
%   'journal = {Australian Journal of Zoology}, ' ...
%   'volume = {62}, ' ...
%   'pages = {448-453}'];
%   eval(['metadata.biblist.' bibkey, '= ''@', type, '{', bibkey, ', ' bib, '}'';']);
%     
%     %
%   bibkey = 'S. Arnall pers. obs.'; type = 'Misc'; bib = ...
%   'Arnall, Sophie <arnals01@student.uwa.edu.au>'; 
%   eval(['metadata.biblist.' bibkey, '= ''@', type, '{', bibkey, ', ' bib, '}'';']);
%   
%     %
%   bibkey = 'G. Kuchling pers. obs.'; type = 'Misc'; bib = [ ... 
%   'Kuchling, Gerald <Gerald.Kuchling@DPaW.wa.gov.au>'
%   eval(['metadata.biblist.' bibkey, '= ''@', type, '{', bibkey, ', ' bib, '}'';']);];
% 

%% Facts
 
F1 = 'All stages experience torpor, and torpor is implemented by letting f switch backwards and forwards from f_zoo to 0';
F2 = 'Torpor occurs approximately late-December until mid-May every year (zoo popn)';
metadata.bibkey.F2 = 'S. Arnall pers. obs.'; 
F3 = 'Mating occurs after torpor in the pond, approximately July-August (zoo popn)';
metadata.bibkey.F3 = 'S. Arnall pers. obs.'; 
F4 = 'Egg laying occurs before torpor, approximately November-December'; 
metadata.bibkey.F4 = 'S. Arnall pers. obs.'; 
F5 = 'Torpor seems required for reproduction';
metadata.bibkey.F5 = 'G. Kuchling pers. obs.'; 
F6 = 'Average mass loss over torpor period is 7.9%, no corresponding significant change to length (0.2mm; 2012-2013 data collected by S. Arnall)';
metadata.bibkey.F6 = 'S. Arnall unpublished data'; 
F7 = 'Some individuals occasionally undergo a half-torpor season due to being used in the public exhibit pond';
metadata.bibkey.F7 = 'S. Arnall pers. obs.'; 
F8 = 'All coupling/mating, entry into torpor, and exit from torpor is artificially imposed by staff at Perth Zoo. Females may have been capable of becoming gravid ealier if given opportunity to mate prior.';
metadata.bibkey.F8 = 'S. Arnall pers. obs.'; 
 
metadata.facts = struct('F1',F1,'F2',F2,'F3',F3,'F4',F4,'F5',F5,'F6',F6,'F7',F7,'F8',F8);
  
%% Discussion points

