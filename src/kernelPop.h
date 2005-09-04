/* 
   


 */


extern "C" {
#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
}
/**
Defines:
*/
#define LOCUSLEN         5
#define ALLELELEN        4
#define NONGENOTYPECOLS  9

///names of parameters in the simulation.  Used as names in lists

#define INTEGERPARAMS    "intparam"
#define SWITCHPARAMS     "switchparam"
#define FLOATPARAMS      "floatparam"
#define DEMOPARAMS       "demography"
#define LOCIPARAMS       "loci"
#define EXPRESSIONPARAMS "expression"
#define INDPARAMS        "individuals"

#define HABNAMES         "habitats"     
#define STAGENAME 	 "stages"       
#define	LNUMNAME	 "locusnum"     
#define	ENUMNAME	 "numepochs"    
#define	CGNAME		 "currentgen"   
#define	CENAME		 "currentepoch" 
#define	FINALAGE	 "totalgens"    
#define	DNUMNAME	 "numdemos"     
#define	MAXLANDNAME	 "maxlandsize"  
#define	NPHENNAME	 "nphen"  
#define RDEMONAME        "randdemo"


#define	ASPECTNAME	 "aspect"  

#define TYPENAME	 "type"    
#define	PLOIDYNAME	 "ploidy"  
#define	TRANSNAME	 "trans"   
#define	RATENAME	 "rate"    
#define	ALISTNAME 	 "alleles" 

#define AINDXNAME        "aindex"  
#define	ABIRTHNAME	 "birth"   
#define	PROPNAME	 "prop"    
#define	STATENAME	 "state"   

#define LOCALDEMNM       "localdem"
#define EPOCHDEMNM       "epochs"

#define LCLSMATNM        "LocalS"
#define LCLRMATNM        "LocalR"
#define LCLMMATNM        "LocalM"

#define RNDCHSNAME       "RndChooseProb"
#define	SGENAME	         "StartGen"     
#define	EXTINCTNAME	 "Extinct"      
#define	CARRYNAME	 "Carry"        
#define	LPNAME		 "Localprob"    
#define SNAME            "S"    
#define	RNAME		 "R"            
#define	MNAME      	 "M"            
#define SKNAME           "seedkern"
#define PKNAME           "pollenkern"

#define RANDEPOCHN       "randepoch"
#define RANDDEMON 	 "randdemo" 
#define MULTPNAME        "multp"

#define SELFRATENAME     "selfing"
#define MINDENSNAME      "mindens"
#define POLLENMUNAME     "pollenmu"
#define POLLENSHAPENAME  "pollenshape"
#define POLLENMU2NAME     "pollenmu2"
#define POLLENSHAPE2NAME  "pollenshape2"
#define POLLENMIXNAME       "pollenmix"

#define SEEDMUNAME       "seedmu"
#define SEEDSHAPENAME    "seedshape"
#define SEEDMU2NAME       "seedmu2"
#define SEEDSHAPE2NAME    "seedshape2"
#define SEEDMIXNAME       "seedmix"

#define SEEDNMUNAME       "seednmu"
#define SEEDNSHAPENAME    "seednshape"
#define SEEDNMU2NAME       "seednmu2"
#define SEEDNSHAPE2NAME    "seednshape2"
#define SEEDNMIXNAME       "seednmix"

#define LEFTXNAME       "leftx"
#define RIGHTXNAME      "rightx"
#define TOPYNAME        "topy"
#define BOTYNAME        "boty"

#define EXPMATNAME      "expmat"
#define HERITABLENAME   "hsq"

///input/output of landscapes to/from metasim lib
extern "C" SEXP read_landscape(SEXP fn);
extern "C" SEXP convert_metasim_to_R(Landscape_space_statistics &L);
extern "C" SEXP write_landscape(SEXP fn, SEXP Rland);
extern "C" void convert_R_to_metasim(SEXP Rland, Landscape_space_statistics &L);
extern "C" SEXP getListElement(SEXP list, const char *str);

///simulations
///run metasim on the landscape a certain number of times
extern "C" SEXP iterate_landscape(SEXP numit, SEXP Rland, SEXP cmpress);
 
extern "C" SEXP populate_Rland(SEXP Rland, SEXP Population_sizes);

///utility functions
///convert a landscape into a format that the weir fst calculations in R can use.
extern "C" SEXP l2w(SEXP Rland, SEXP numind);
extern "C" SEXP num_demo_cols();
extern "C" SEXP clean_landscape(SEXP Rland);

///output functions
///convert landscapes to various formats
///All take a filename, a landscape, and a number of indiviudals to sample per habitat
extern "C" SEXP writeGDA(SEXP fn, SEXP Rland, SEXP ni);
extern "C" SEXP writeArlequinDip(SEXP fn, SEXP Rland, SEXP ni);
extern "C" SEXP writeArlequinHap(SEXP fn, SEXP Rland, SEXP ni);
extern "C" SEXP writeBIOSYS(SEXP fn, SEXP Rland, SEXP ni);
extern "C" SEXP writeGenPop(SEXP fn, SEXP Rland, SEXP ni);
extern "C" SEXP writeR(SEXP fn, SEXP Rland, SEXP ni);
extern "C" SEXP writeMigrateDip(SEXP fn, SEXP Rland, SEXP ni);
extern "C" SEXP writeReRat(SEXP fn, SEXP Rland, SEXP ni);


extern "C" SEXP test();
