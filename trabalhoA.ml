let has = Hashtbl.create 1

let rec f1 n =
    let rec sum k b =
        if (b-2) < k then (0,0) else (**Aqui irá ser feita a parte do sumatorio *)
        let (trigo,trigo1) = f1 k in
        let (pao,pao1) = f1 (n-k-1) in
        let (up,up1) = sum (k+1) b in (**esta função serve para fazer a incrementação do proprio sumatorio *)
        (trigo*pao+up,trigo1+pao1+up1)(**segue a mesma estrutura das outras funções mas desta vezes não incrementamos +1 porque isso já sera feito na parte principal *)
    in
    match n with
     0 -> (1,1)
     |1 -> (2,1)
     |_ -> 
        let (res1, quant1) = f1 (n-1) in
        let (res2, quant2) = sum 1 n in
        ((3*res1)+res2, quant1+quant2+1)
    ;;

let rec f2 n = 
    match n with (**Usamos match n with em vez de if else, onde as barras servem para continuar o raciocinio *)
     0 -> (1,1)
     |1 -> (2,1)
     |_ ->
    let (res1, quant1) = f2 (n-1) in 
    let (res2, quant2) = f2 (n-2) in
    let first = (6*n-3) * res1 in
    let second = first/(n+1) in
    let third = (n-2) * res2 in
    let fourth = third / (n+1) in
    let last = second - fourth in
    (last, quant1+quant2+1) (**O primeiro parametro é o valor do calculo. O segundo parametro vai ser a quantidade de vezes que a função vai ser executada ou seja o +1 simplesmente é incrementação *)
    ;; 

let rec f2_z n =
    match n with
     0 -> (Z.of_int 1,1)
     |1 -> (Z.of_int 2,1)
     |_ -> if Hashtbl.mem has n then Hashtbl.find has n else (**Hastbl.mem serve para procurar se o valor x encontra se na tabela has e se assim for ele vai usar Hastbl.find e devolve o valor que lá se encontra *)
            let (res1,quant1) = f2_z (n-1) in
            let (res2,quant2) = f2_z (n-2) in
            let first = Z.mul (Z.of_int (6*n-3)) res1 in
            let second = Z.div first (Z.of_int (n+1)) in
            let third = Z.mul (Z.of_int (n-2)) res2 in
            let fourth = Z.div third (Z.of_int (n+1)) in 
            let last = Z.sub second fourth in
            Hashtbl.add has n (last ,quant1+quant2+1); (**Hastbl.add com isto vamos adicionar no espaço n o resultado da conta da direita *)
            Hashtbl.find has n; (**No final queremos que ele devolva o valor que se contrar no n ((x,y)) caso não façamos este caso estamos a passar unit() em vez de (zarith,int)*)
    ;;

let main () =
    let (a, b) = Scanf.scanf " %d %d" (fun c d -> (c, d)) in
    if (a >= 0 && a <= 20 && b >= 0 && b <= 10000) then (**Se os valroes se encontrarem dentro dos limites o progrma corre normal caso contrario o programa acaba sem acontecer nada *) 
        let (res1,quant1) = f1 a in
        let (res2, quant2) = f2 a in
        let (res2_z,quant2_z) = f2_z b in
        Printf.printf "%d %d\n" res1 quant1; (**imprimimos o a quantidade e o numero de vezes que a função foi chamada *)
        Printf.printf "%d %d\n" res2 quant2; 
        Z.print res2_z;
        Printf.printf "\n"
    ;;

main();

(**Exemplos:
    Input:  16 2342
    Output: 20927156706 470832
            20927156706 3193
            595861385285715585740633888786355928483886435863072670035116258843260394350229851431919538052597100795255746327524888023989343232239844285607984704164038624133290726152006962687347096770159682168661472669052790555830957506103167092507534086160394241349003233834836942628444330567892442383026708672430010295400098819355015788530501890652325462932864772284086522288294871354020807051205647392680362531812422720362313983917955120393577664994745293067911306484857547775930812508851654558453544329604590177151333837481651437558170324336161206606104755396321223551684867105153053112694979536320754223113724261107450977668068712283829860330971678745615962330411775048071562577811584112669050174964741139854512529665782073048866056254853981437007902866365749077913566392926117837981142369547143500432955976958602225508340068231988460153756674723123424358751176714704223043535059524910583049725114513037558195182138089649902606571237983607465397675859929039585166676778890451881253560305387455886010325872614648811771218716514695826392280184565567848202993417557131100573601039833740364991723626652035450283604142583290268524450075978640183562647582780245808183011406537547332801649850635781346349663972462697521580320997492123105391615627606130769500609648408213281142439826919683639273222486618781747802599724527140247577722754988269502132051926679400965738653218989148146617327815229359843031389219031704421651692815229907359329241403077751388227416638614182322903366650005551473779861761451673101357859681495353887101239522767226480124765448939885966836611300438726192131761041550504339032676474156745266444194738244127896530816053800308646228683668122329259532208206121060290634704871678430624829944211901228008985330415250152574431416614949302983838363804838236991071805494977173435483380046
            
Trabalho realizado por:
    Gabriel Maria nº47922
    Gonçalo Costa nº48243
    *)
