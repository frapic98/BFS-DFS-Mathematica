(* ::Package:: *)

(* :Title: BfsDfs *)
(* :Context: BfsDfs` *)
(* :Author: Francesco Saverio Beccafichi, Maria Cidoncha Torvisco, Francesco Piconi, Alessandro Pistola, Carlo Zazzetti*)
(* :Summary: versione base dell'implementazione degli algoritmi BFS(Breadth-first search) e DFS(Depth-first search) *)
(* :Copyright: B-D_fs 2022 *)
(* :Package Version: 31, Maggio 2022 *)
(* :Mathematica Version: 13 *)
(* :Sources: biblio *)


BeginPackage["BfsDfs`"];
generaEsercizio::usage="generaEsercizio [] 
	Genera il tutorial e l'interfaccia grafica per interagire con l'esercizio"


(* Parametri per la creazione del grafo *)

(* Numero di vertici *)
nVertici=12;

(* Probabilit\[AGrave] che un nodo abbia un arco uscente *)
probArcoUscente=0.35;

(* Seed di default *)
seed=1;

(* Label utilizzate per restituire un feedback durante lo svolgimento degli esercizi *)
corretto=Style[Text["Risposta corretta!"],FontColor->Green, FontSize->30];
sbagliato=Style[Text["Attenzione hai sbagliato!"],FontColor->Red, FontSize->27];

(* Variabile booleana: indica se la modalit\[AGrave] attuale \[EAcute] "Esempio" o "Esercizio" *)
modalitaEsempio=0;

(* Variabile booleana: indica se la risposta fornita dall'utente \[EAcute] corretta o sbagliata *)
rispostaCorretta=1;


(* Impostazioni oer ricercare nel filepath i file audio utilizzati per restituire un feedback all'utente *)
SetDirectory[NotebookDirectory[]]
right=Audio["Resources/rispostaCorretta.mp3"];
wrong=Audio["Resources/rispostaSbagliata.mp3"];


(*Funzione principale che richiama le precedenti per creare un grafo e calcolare i nodi dell'algoritmo di default(BFS), per generare l'interfaccia grafica *)
generaEsercizio[]:= Return[
grafo = generaGrafo[seed, nVertici, probArcoUscente]; 
nodi = calcolaNodiBfs[grafo];

(* All'interno della manipulate sono presenti tutti gli elementi dell'interfaccia grafica *)
Manipulate[
	(* La GraphicsRow unita alla HighlightGraph visualizza il grafo*)
	GraphicsRow[{HighlightGraph[grafo,{0,nodi[[;;i]]},GraphHighlightStyle->"Thick"]}, ImageSize->500],
	(* All'interno della Row stampiamo una InputField e un bottone per inserire un seed a scelta *)
	Row[{Text["Seed: ", BaseStyle->{Medium}],
		InputField[Dynamic[seed],Number],
		Button["Conferma", Do[{grafo=generaGrafo[seed,nVertici, probArcoUscente],i=1, modalitaEsempio=0},1]]
	}],

	Delimiter,

	(* Il seguente If permette di stampare all'occorrenza gli elementi dell'interfaccia in base alla modalit\[AGrave] selezionata("Esempio" di default) *)
	Dynamic@If[Modalita=="          Esempio         ",
	
	(* Se la modalit\[AGrave] selezionata \[EGrave] quella di "Esempio" allora si visualizza lo slider che permette di muoversi all'interno dei cammini BFS o DFS. 
	Nel caso opposto lo slider viene disabilitato *)
	Column@{Row[{Text["Passo: ", BaseStyle->{Medium}],Control[{{i,1, ""}, 0, Dynamic[Length[VertexList[grafo]]], 1}]}]}, i=1;
	Row@{Text["Passo: ", BaseStyle->{Medium}],Control[{{i,1, ""}, 0, Length[VertexList[grafo]],1, Enabled->False}]}],
	
	(* Row of Buttons utilizzata per sciegliere la modalit\[AGrave] *)
	{Modalita ,{"          Esempio         ","          Esercizio         "}, BaseStyle->{Medium}},
	
	Delimiter,
	
	(* Row of Buttons utilizzata per sciegliere l'algoritmo di visita *)
	{Algoritmo,{"\t BFS \t","\t  DFS\t"}, BaseStyle->{Medium}},
	(* Viene calcolata la lista dei nodi in base all'algoritmo di visita selezionato *)
	Dynamic@If[Algoritmo=="\t BFS \t",Row[{nodi=calcolaNodiBfs[grafo];}],Row[{nodi=calcolaNodiDfs[grafo];}], Row@{}],
	Delimiter,
 
	Row[{
		(* In modalit\[AGrave] Esempio vengono visualizzati i bottoni "Avanti" e "Indietro" per muoversi di un solo passo 
		da un nodo all'altro secondo l'algoritmo di visita selezionato *)
		Dynamic@If[Modalita == "          Esempio         ",
			Button["Avanti",Dynamic@If[i <= Length[nodi]-1,i=i+1,i=1], ImageSize->150], Row@{}],
		Dynamic@If[Modalita == "          Esempio         ", Button["Indietro",Dynamic@If[i>1, i = i-1 ,i = 1], ImageSize->150],
		(* Se la modalit\[AGrave] selezionata \[EGrave] "Esercizio" non vengono visualizzati i bottoni "Avanti" e "Indietro", ma la Input Field 
		per inserire il prossimo nodo da visitare in base all'algoritmo di visita selezionato *)
			Row[{Text["Prossimo nodo: ", BaseStyle->{Medium}],Dynamic[InputField[Dynamic[x], Number, ImageSize->205]]}]]
	}],
	
	(* In entrambi viene visualizzato il bottone "Ripristina". Nel caso della modalit\[AGrave] "Esercizio" viene mostrato anche il bottone "Conferma": tale bottone controlla 
	se il valore inserito dall'utente nella Input Field \[EGrave] corretto o meno. In entrambi i casi stampa um messaggio 
	di feedback, e se la risposta \[EGrave] corretta avanza al prossimo nodo *)
	Dynamic@If[Modalita=="          Esempio         ",
		Button["Ripristina",i=1,ImageSize->300],
		Column@{
			Button["Conferma", Dynamic@If[x==nodi[[i+1]], Do[{rispostaCorretta=1, modalitaEsempio=1, EmitSound[right], i= i+1},1],
				Do[{rispostaCorretta=0, modalitaEsempio=1, EmitSound[wrong]},1]]],
			Button["Ripristina", Do[{i=1, modalitaEsempio=0},1], ImageSize->300],
			If[rispostaCorretta==1 && modalitaEsempio == 1,corretto],
			If[rispostaCorretta==0 && modalitaEsempio == 1,sbagliato]
		}],
	ControlPlacement->Left]
]


Begin["`Private`"];


(* Genera un grafo pseudo-randomico partendo da un seed, un numero di vertici e una probabilit\[AGrave].
Fa uso della built-in BernoulliGraphDistribution(per approfondire guardare Bibliografia).
Viene effettuato un controllo per verificare che l'albero generato sia o meno connesso (attraverso la built-in "ConnectedGraphQ").
In caso quest'ultimo non sia connesso, si utilizza la built-in di mathematica "ConnectedGraphComponents" per ottenere 
una lista delle componenti cos\[IGrave] da essere in grado di rimuovere la componente non connessa e ritornare 
correttamente un grafo connesso.*)
generaGrafo[seed_,nVertici_,probArcoUscente_]:= Module[{x=seed,n=nVertici,p=probArcoUscente},
SeedRandom[x];
g=RandomGraph[BernoulliGraphDistribution[n,p],VertexLabels->Automatic];
If[ConnectedGraphQ[g],g,Graph[Part[ConnectedGraphComponents[g],1]]]];


(* Calcola e ritorna la lista dei nodi dell'albero risultante dall'esecuzione dell'algoritmo BFS, tramite la built-in BreadthFirstScan (vedi Bibliografia) *)
calcolaNodiBfs[igrafo_] := Module[{grafo= igrafo},
	nodiBfs = List[]; 
	BreadthFirstScan[grafo,Part[VertexList[grafo],1],{"FrontierEdge"->Sow,"PrevisitVertex"->(AppendTo[nodiBfs,#]&)}];
	Return[nodiBfs];
]

(* Calcola e ritorna la lista dei nodi dell'albero risultante dall'esecuzione dell'algoritmo DFS, tramite la built-in DepthFirstScan (vedi Bibliografia) *)
calcolaNodiDfs[igrafo_] := Module[{grafo= igrafo},
	nodiDfs = List[]; 
	DepthFirstScan[grafo,Part[VertexList[grafo],1],{"FrontierEdge"->Sow,"PrevisitVertex"->(AppendTo[nodiDfs,#]&)}];
	Return[nodiDfs];
]





End[];


EndPackage[];
