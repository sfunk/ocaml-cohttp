(* Underlying assumption here:
   All 20 runs of one function do the same number of Async-invocations
   (this seems to be justified by the analysis-tool)
   Need to explain Networking sockets etc.

   For major allocations, want to ignore outliers that result from a major-gc

   runtime overhead negligible for this measurement


   for measurements for minor and major-gcs:
   test_net_event without any pa_events has no minor or major-gcs
   with full 3 fields, 112 invocations,
   still 0 major-gcs but 3 in 30 minor gcs
   this shows, it is too much!

   minor_heap_size is 1_000_000 words, so
   1_000_000 * 64 B = 64 MB

   with 112 invocations, 71487 is ~4MB of minor allocations!
*)


open Core.Std
module Ascii_table = Textutils.Ascii_table

module Data = struct

    (* Tuples of the form:
       (minor_allocations,major_allocations, invocation, runtime) *)


  let field1_7invocations = (
    [38186;19352;18603;18679;18418;18676;18299;18290;18473;18704;19472;19009;18437;18694;18938;18697;18690;18164;18676;18601;18840;19226;19069;18452;18416;18421;18680;18171;18414;18676],
    [59;46;28;28;28;28;28;28;28;28;28;28;28;28;28;28;28;28;28;28;28;28;28;73503;28;28;28;28;28;28],
    7,
    [89.51902;80.72710;92.91911;86.08603;79.31614;81.36702;79.74100;88.24301;82.85499;87.63719;79.79703;80.03497;149.14083;81.37703;83.06193;83.35686;83.34112;80.40380;80.06692;83.87589;81.44903;81.42900;83.86707;84.22208;80.34086;83.43697;81.07996;112.31804;82.12185;85.28900] 
  ) 

  let field1_54invocations = (
    [67959;49301;48374;48306;48066;48301;48551;48306;48305;48347;48306;48855;50036;48302;48826;48309;48338;48302;48309;48582;48309;48308;48302;48306;51377;48305;48301;48593;48066;48306],
    [59;46;28;28;28;28;28;28;28;71127;28;28;28;28;28;28;28;28;28;28;28;28;28;28;28;28;28;28;28;5982],
    54,
    [84.34582;98.03200;79.66304;81.35009;88.07397;88.97591;80.37281;83.41813;86.49707;89.84089;80.33895;84.01990;89.76603;96.18807;79.95892;80.48201;82.14498;110.00013;97.74804;79.99516;101.78113;86.68208;81.94804;88.04107;81.78306;83.42719;84.88989;81.15506;80.59907;83.39190])

  let fields0 = (
    [33527;14789;14215;14143;13882;14147;14006;14143;13878;13878;14184;14696;14340;13882;13989;14162;14147;13878;13639;13885;13878;13882;14162;14965;14874;14069;13639;13878;13882;13882],
    [59;46;28;28;28;28;28;28;28;28;28;28;28;28;28;28;28;28;28;28;28;28;28;28;28;28;28;28;28;28],
    0,
    [81.61592;80.50799;79.86498;78.85480;80.03807;79.98919;79.57506;83.42981;79.10609;78.79901;76.82896;84.10692;79.57911;81.52604;79.97012;131.21080;89.88786;86.89213;81.90513;108.67405;100.44789;101.74799;86.31706;99.51115;84.96881;87.75997;95.32714;81.34198;79.55408;80.72710]  
)

  let fields4_112invocations = (
    [104787;86393;85294;85224;85355;85661;85485;85351;85359;85108;88411;85350;85350;85350;85593;85351;85356;85355;85352;85346;85104;86419;86151;87246;85114;85345;85361;85352;85111;85103],
    [59;46;28;28;28;70315;28;28;28;28;28;28;28;28;28;28;4022;28;28;28;28;28;28;28;28;28;28;28;3914;28],
    112,
    [130.09000;80.54113;86.95102;80.63316;80.00088;82.43895;86.85184;79.84400;82.40604;79.50091;79.79417;78.68695;80.50895;80.21498;79.38004;80.38998;80.63102;80.33490;141.76106;87.26215;79.23198;81.31504;80.15013;78.33219;80.38211;80.80316;82.80993;86.79485;81.74300;79.33497]
)

  let fields2_61invocations = (
    [72473;53800;52884;52811;52815;52566;52936;52812;52881;53070;53096;53889;54542;52853;53051;52820;53342;53097;52809;52803;52808;52931;52801;53096;55587;52815;52809;52818;52812;52804], 
    [59;34;28;28;28;28;28;28;70677;28;28;28;28;28;28;28;28;28;28;28;28;28;28;28;28;28;28;5519;28;28],
    61, 
    [114.23683;89.39099;83.48298;86.40099;83.59003;85.58416;79.48112;87.29815;83.43697;80.11603;90.65795;82.48687;87.49890;84.10692;85.10399;83.37688;86.21693;80.62100;90.12604;89.59198;80.63293;81.08592;80.02996;82.86095;84.31602;80.93691;81.47287;82.38101;86.12585;83.11701]
  )

  let fields2_54invocations = (
    [68009;49092;48173;48344;48627;48347;48464;48340;48294;48385;48104;49140;50091;48344;48340;48104;48616;48340;48347;48624;48344;48347;48340;49310;50330;48347;48629;48344;48349;48352],
    [59;46;28;28;28;28;28;28;28;71127;28;28;28;28;28;28;28;28;28;28;28;28;28;28;28;28;28;28;28;5982],
    54,
    [84.34582;98.03200;79.66304;81.35009;88.07397;88.97591;80.37281;83.41813;86.49707;89.84089;80.33895;84.01990;89.76603;96.18807;79.95892;80.48201;82.14498;110.00013;97.74804;79.99516;101.78113;86.68208;81.94804;88.04107;81.78306;83.42719;84.88989;81.15506;80.59907;83.39190]
  )

  let fields2_105invocations = (
    [100588;81864;80881;80807;80813;80846;80694;80812;81198;80804;81471;82538;81101;80926;81089;80570;81342;81093;80566;80801;80808;80809;81601;82761;80558;80812;81097;80566;80812;80803],
    [59;46;28;28;28;70477;28;28;28;28;28;28;28;28;28;28;28;3901;28;28;28;28;28;28;28;28;28;28;28;28],
    105,
    [123.00611;88.08017;80.64699;87.66699;85.50382;88.42206;88.70387;81.23922;91.14695;84.76496;82.85499;88.66000;82.01289;111.50694;81.44212;85.05893;80.62482;80.45888;82.99303;84.46002;83.10008;80.49297;81.81596;162.65011;82.21602;81.20394;80.97100;79.50187;80.86991;79.81205]
  )


  let fields2_58invocations =(
    [70317;51943;50735;51167;51176;51164;51291;50906;51215;50925;50659;51960;51630;51171;51164;51051;51440;50901;51291;50903;51402;51168;51720;51586;51164;51171;50664;51168;51179;50962], 
    [59;46;28;28;28;28;28;28;70839;28;28;28;28;28;28;28;28;28;28;28;28;28;28;28;28;28;28;28;5910;28],
    58,
    [137.16006;82.37910;90.92307;88.56201;107.79190;81.20394;98.64902;80.01900;81.92301;79.42891;88.21011;80.30105;80.07097;91.60995;87.22711;80.94096;88.40203;89.98203;80.96099;80.24597;89.86902;83.03905;97.67079;80.52397;86.77793;86.13896;88.82403;87.73088;79.31995;80.72901])

  let field1_51invocations =(
    [65814;47364;46513;46149;46401;46665;46284;46665;46668;46460;46399;46953;47113;46665;46632;46440;46940;46394;46665;46400;46664;46668;46661;47457;47121;46421;46400;46671;46156;46421],
    [59;46;28;28;28;28;28;28;28;70886;28;28;28;28;28;28;28;28;28;28;28;28;28;28;28;28;28;28;28;28],
    51,
    [102.92411;81.07686;86.69186;80.71589;80.92785;98.69194;83.17780;87.68892;78.90296;87.31198;81.28786;84.04708;88.45997;80.37686;86.56311;82.18098;82.80921;86.82990;80.15203;81.91609;79.99206;81.00319;86.21502;80.36780;79.54192;81.05683;90.44600;80.48201;80.16515;79.18310]
)

  let data = 
    let temp = [ fields0;
	       field1_7invocations;
	       field1_54invocations;
	       fields4_112invocations;
	       fields2_61invocations;
	       fields2_54invocations;
	       fields2_105invocations;
	       fields2_58invocations;
	       field1_51invocations;
	     ]
    in
    List.map temp ~f:(fun (allocs,major_allocs,invocs,runtime) ->
      let mean =  ((List.fold ~f:(+) ~init:0 major_allocs) / 
		      (List.length major_allocs)) 
      in
      let major_allocs = List.sort major_allocs ~cmp:Int.compare in
      let new_major_allocs = match List.group major_allocs
	~break:(fun a b -> (Float.of_int b) /. (Float.of_int mean) > 1.25) with
	  | (a::rest) -> printf "Sorted out:%d\n" (List.length rest); a
	  | [] -> assert false
      in
      (allocs,new_major_allocs,invocs,runtime)
    )
	
end

let () = 
  let col ~name f = 
    Ascii_table.Column.create name ~show:`Yes f
  in 
  let mean list = ((List.fold ~f:(+) ~init:0 list) / (List.length list)) in
  let mean_float list = ((List.fold ~f:(+.) ~init:0. list) /. 
			    Float.of_int (List.length list)) 
  in
  let baseline_alloc = 
    List.map Data.data ~f:(fun (alloc,_,_,_) -> alloc)
    |> List.map ~f:mean
    |> List.fold ~init:Int.max_value ~f:Int.min
  in
  let baseline_major_alloc = 
    List.map Data.data ~f:(fun (_,major_allocs,_,_) -> major_allocs)
    |> List.map ~f:mean
    |> List.fold ~init:Int.max_value ~f:Int.min
  in
    
  let baseline_time = 
    List.map Data.data ~f:(fun (_,_,_,runtime) -> runtime)
    |> List.map ~f:mean_float
    |> List.fold ~init:Float.infinity ~f:Float.min
  in
  printf "baseline_alloc: %d\n" baseline_alloc;	    
  printf "baseline_major_alloc: %d\n" baseline_major_alloc;	    
  printf "baseline_time: %.2f\n" baseline_time;	    
  let columns = [ col "Invocations" (fun (_,_,invocs,_) -> string_of_int invocs);
    col "minor" (fun (allocs,_,_,_) ->
      string_of_int (mean allocs));
    col "minor from baseline" (fun (allocs,_,_,_) ->
      string_of_int ((mean allocs) - baseline_alloc));
    col "alloc / invoc" (fun (allocs,_,invoc,_) ->
      if invoc=0 then "n/a" 
      else 
      string_of_int (((mean allocs) - baseline_alloc) / invoc) );

    col "major" (fun (_,allocs,_,_) ->
      string_of_int (mean allocs));
    col "major from baseline" (fun (_,allocs,_,_) ->
      string_of_int ((mean allocs) - baseline_major_alloc));
    col "alloc / invoc" (fun (_,allocs,invoc,_) ->
      if invoc=0 then "n/a" 
      else 
      string_of_int (((mean allocs) - baseline_major_alloc) / invoc) );

    col "Mean runtime (ms)" (fun (_,_,_,runtime) ->
      sprintf "%.3f" (mean_float runtime));
    col "From Baseline time" (fun (_,_,_,runtime) ->
      sprintf "%.2f" ((mean_float runtime) -. baseline_time));
    col "Per invocation time" (fun (_,_,invoc,runtime) ->
      if invoc=0 then "n/a" 
      else 
      sprintf "%.3f" (((mean_float runtime) -. baseline_time) /. (Float.of_int invoc)) );
  ]
  in
  Ascii_table.output ~oc:stdout columns Data.data
