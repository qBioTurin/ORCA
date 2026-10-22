# ORCA unificata — confronto e integrazione

Data: 22 ottobre 2026. Versione di sviluppo: `2.1.0.9000`.

La cartella `ORCA-merged` è una copia indipendente del progetto, senza la cronologia
Git, i file personali di RStudio o la cronologia della console. Nessun file delle
due sorgenti è stato modificato durante il merging.

## Sorgenti

- Base: cartella originale `ORCA` (ORCA attuale, incluse le correzioni e i test sviluppati in questa conversazione).
- Integrazioni: cartella originale `ORCAv3/ORCA`.
- Le due sorgenti dichiarano entrambe versione 2.0; il nome ORCAv3 non è stato
  interpretato come indicazione che ogni suo file fosse più aggiornato.
- `MERGE_SOURCES.json` registra i percorsi e gli hash dei principali file sorgente.

## Confronto e scelte

| Area | Decisione nella copia unificata |
|---|---|
| UI | Tema, homepage e istruzioni della versione attuale; aggiunta del menu FACS raw. |
| PCR | Conservate baseline comuni/specifiche per campione, gestione dei tempi e grafici attuali. |
| Microscopia | Conservate analisi su valori originali/percentuali, colori e annotazioni statistiche attuali. |
| Integrazione omica | Conservati i rami WB e PCR della versione attuale. |
| Figure | Conservati editor condiviso, ordine asse X ed esportazione multiformato attuali. |
| FACS da Excel | Conservati workflow attuale e correzioni allo stato vuoto, inizializzazione e conversione delle percentuali. |
| FACS raw | Integrati importazione FCS, visualizzazione, gate poligonali, gerarchie, riutilizzo, normalizzazione e download da ORCAv3. |
| Citotossicità | Integrata personalizzazione separata dei due grafici usando l'editor condiviso attuale; conservato il grafico combinato per l'esportazione dell'analisi. |
| BCA | Entrambe le direzioni del rapporto sono selezionabili: Target / baseline (default attuale) e Baseline / target (ORCAv3). Le colonne vengono ricalcolate al cambio della quantità di lisato. |
| Statistiche | Conservata l'architettura attuale; corretto il collegamento tra i risultati della funzione statistica e i pannelli chiamanti. |
| WB, ELISA, endocitosi, Dataverse, simulazione | Conservata la base attuale; nessuna sostituzione con blocchi equivalenti o meno estesi di ORCAv3. |
| Avvio | Conservati porta e modalità Docker configurabili, senza dipendenza dalla funzione interna di RStudio. |
| Docker | Build configurata per installare il checkout locale unificato, anziché GitHub main; corretto l'argomento `inDocker`. Build non eseguita. |
| Dati di esempio | Identici nelle due sorgenti, copiati dalla base. |

## Correzioni necessarie all'integrazione

- La funzione di lettura FCS gestisce file mancanti/non validi e nomi duplicati.
- Le gerarchie riutilizzate sono applicate in ordine di profondità; ogni gate
  figlio filtra la popolazione del parent. Una popolazione vuota resta un gate valido.
- La tabella raw viene calcolata indipendentemente dal rendering della UI.
- Il trasferimento raw → gerarchia preserva la prima riga, che è un campione,
  e inizializza il livello zero anche se la scheda era già stata aperta.
- Il nuovo caricamento raw resetta gerarchie e associazioni della sessione.
- Il pulsante statistiche FACS gestisce condizioni mancanti/repliche insufficienti,
  usa la firma corretta di `testStat.function` e produce una tabella per gate.
- I risultati multigruppo vengono letti dal campo `MulvTest` effettivamente
  restituito dalla funzione comune, invece del campo inesistente `test`.
- Le funzioni condivise vengono caricate dalla stessa cartella del server,
  evitando di mescolare la copia unificata con un'altra installazione.
- La libreria flowCore è dichiarata tra le dipendenze; la UI mantiene la precedenza
  delle librerie grafiche/dplyr e il filtro PCR interessato è qualificato.

## Verifica

Dalla radice della copia unificata:

```sh
Rscript tests/manual/merged-features.R
```

La suite comprende i test di `smoke-app.R` e i nuovi scenari di integrazione:
22 scenari e 177 verifiche. Copre navigazione e output iniziali di 31 schede,
importazioni di esempio, gating Excel, importazione FCS valida/non valida,
gate raw annidati, riutilizzo su un secondo campione, trasferimento della
gerarchia, esportazione Excel raw, statistiche FACS su un esempio noto,
entrambe le formule BCA e apertura della personalizzazione di citotossicità.

È stata inoltre verificata l'installazione con `R CMD INSTALL` in una libreria
separata sotto `/tmp`; l'installazione ORCA dell'utente non è stata sostituita.
Restano avvisi preesistenti di sovrapposizione dei nomi importati tra pacchetti.

## Limiti e convenzioni conservate

- Test Shiny lato server: nessun collaudo completo delle interazioni JavaScript
  nel browser, dei servizi Dataverse o dei container Docker.
- Non è una validazione scientifica completa di tutte le analisi.
- La selezione automatica dei test statistici resta quella delle sorgenti;
  non sono state introdotte nuove regole inferenziali o correzioni multiple.
- La normalizzazione FACS raw mantiene la convenzione ORCAv3: rapporto dei
  conteggi × 100, arrotondamento e ricalcolo delle percentuali. Modifica i conteggi
  nella sessione; il suo significato scientifico e i casi limite richiedono
  validazione dedicata prima dell'uso in produzione. Il riutilizzo dei gate
  e la normalizzazione non sono la stessa operazione.
- La compatibilità di ogni vecchio RDS non è stata verificata.
- Licenza e ORCID segnaposto di DESCRIPTION sono quelli originali; nessuna
  decisione editoriale o di licenza è stata presa nel merging.

## Avvio locale

Aprire `ORCA.Rproj` in questa cartella e, in una nuova sessione R:

```r
pkgload::load_all()
ORCA.run()
```

Per una build Docker della copia unificata (non collaudata in questa sessione):

```sh
docker build -f inst/Docker/ShinyApplication/Dockerfile -t orca-merged:local .
docker run --rm -p 3838:3838 orca-merged:local
```

L'immagine remota predefinita del launcher Docker originale non contiene
necessariamente questo merging: per questo checkout usare l'immagine locale.

## Pubblicazione Git

Nel branch `merge/orca-unified` il progetto unificato occupa la radice del
repository. I riferimenti a `ORCA-merged` indicano la cartella locale originaria.
La chiave API Dataverse e i file personali RStudio non sono inclusi.
