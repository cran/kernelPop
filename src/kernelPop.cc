/* 
Allan Strand 9/17/01   
*/

#include <Landscape_space.h>
#include <FastAllele.h>
#include <FastSeqAllele.h>
#include <iostream>
#include <fstream>
#include <kernelPop.h>
#include <vector>

extern "C" {

  /* get the list element named str, or return NULL */
  /*This code comes from the R-exts documentation */
 
  SEXP getListElement(SEXP list, const char *str)
  {
    SEXP elmt = R_NilValue, names = getAttrib(list, R_NamesSymbol);
    int i;
    
    for (i = 0; i < length(list); i++)
      if(strcmp(CHAR(STRING_ELT(names, i)), str) == 0) {
	elmt = VECTOR_ELT(list, i);
	break;
      }
    return elmt;
  }
  
  void R_to_metasim_ints(SEXP inlist, Landscape_space_statistics &L)
  {
    L.sethabs((asInteger(getListElement(inlist,HABNAMES))));
    L.setstages((asInteger(getListElement(inlist,STAGENAME  ))));
    L.setepochs((asInteger(getListElement(inlist,ENUMNAME   ))));
    L.setCgen((asInteger(getListElement(inlist,CGNAME     ))));
    L.setCepoch((asInteger(getListElement(inlist,CENAME     ))));
    L.setgens((asInteger(getListElement(inlist,FINALAGE   ))));
    L.setndemo((asInteger(getListElement(inlist,DNUMNAME   ))));
    L.setranddemo((asInteger(getListElement(inlist,RDEMONAME))));
    L.setnphen((asInteger(getListElement(inlist,NPHENNAME))));
    L.setMaxLandSize((asInteger(getListElement(inlist,MAXLANDNAME))));
  }
  
  void R_to_metasim_switches(SEXP inlist, Landscape_space_statistics &L)
  {
    
    L.assignRandEpoch((asInteger(getListElement(inlist,RANDEPOCHN))));
    L.setranddemo((asInteger(getListElement(inlist,RANDDEMON))));
    L.setmultp(asInteger(getListElement(inlist,MULTPNAME)));
  }

  void R_to_metasim_float(SEXP inlist, Landscape_space_statistics &L)
  {
    double s;
    s = asReal(getListElement(inlist,SELFRATENAME));
    L.setself(s);
    ///    Rprintf("selfrate: %g \n",s);
    L.setseed_mu((asReal(getListElement(inlist,SEEDMUNAME))),(asReal(getListElement(inlist,SEEDMU2NAME))),0,0);
    L.setseed_shape((asReal(getListElement(inlist,SEEDSHAPENAME))),(asReal(getListElement(inlist,SEEDSHAPE2NAME))),0,0);
    L.setseed_mix((asReal(getListElement(inlist,SEEDMIXNAME))),0);
    
    L.setpollen_mu((asReal(getListElement(inlist,POLLENMUNAME))),(asReal(getListElement(inlist,POLLENMU2NAME))));
    L.setpollen_shape((asReal(getListElement(inlist,POLLENSHAPENAME))),(asReal(getListElement(inlist,POLLENSHAPE2NAME))));
    L.setpollen_mix((asReal(getListElement(inlist,POLLENMIXNAME))));

    L.setaspect((asReal(getListElement(inlist,ASPECTNAME))));
    L.setmindens((asReal(getListElement(inlist,MINDENSNAME))));
  }

  void R_to_metasim_expression(SEXP inlist, Landscape_space_statistics &L)
  {
    int i,j,np,nl;
    double *hsq;

    hsq = (double *) R_alloc(long(L.getnphen()), sizeof(double));

    L.setexpression();

    np = L.getnphen();


    nl = L.getloci();
    hsq = REAL(coerceVector(getListElement(inlist,HERITABLENAME),REALSXP));
    for (j=0;j<np;j++)
      {
	L.setheritability(j,hsq[j]);
      }

    for (i=0;i<nl;i++)
      {
      for (j=0;j<np;j++)
	{
	  L.setexpmatel(i,j,REAL(coerceVector(getListElement(inlist,EXPMATNAME), REALSXP))[i*np+j]);
	}
      }
  }

  void R_to_metasim_demography(SEXP inlist, Landscape_space_statistics &L)
  {
    int e,i=0,j=0,d=0,k=0;
    int en,ld, sz, estrt;
    double epr;

    double *ev, *dv, *lx, *rx, *uy, *ly;
    int *kv;

    
    ld= length(getListElement(inlist,LOCALDEMNM)); ///number of local demos
    en = length(getListElement(inlist,EPOCHDEMNM));///number of epochs


     kv = (int *) R_alloc(long(L.gethabs()), sizeof(int));
     ev = (double *) R_alloc(long(L.gethabs()), sizeof(double));
     dv = (double *) R_alloc(long(ld), sizeof(int));

     lx = (double *) R_alloc(long(L.gethabs()), sizeof(double));
     rx = (double *) R_alloc(long(L.gethabs()), sizeof(double));
     uy = (double *) R_alloc(long(L.gethabs()), sizeof(double));
     ly = (double *) R_alloc(long(L.gethabs()), sizeof(double));
     
     
    SEXP Evec = getListElement(inlist,EPOCHDEMNM);
    PROTECT(Evec);
    for (e=0;e<en;e++)  
      {
	SEXP Demov = VECTOR_ELT(Evec,e);
	PROTECT(Demov);

	epr = asReal(getListElement(Demov,RNDCHSNAME));
	estrt = asInteger(getListElement(Demov,SGENAME));  
	L.setepochprob(e,epr);
	L.setepochstart(e,estrt);
	
	///Vital Vectors:  extinctions/carry capacity

	ev = REAL(coerceVector(getListElement(Demov,EXTINCTNAME),REALSXP));
	kv = INTEGER(coerceVector(getListElement(Demov,CARRYNAME),INTSXP));


	///locations of habitats
	lx = REAL(coerceVector(getListElement(Demov,LEFTXNAME),REALSXP));
	rx = REAL(coerceVector(getListElement(Demov,RIGHTXNAME),REALSXP));
	uy = REAL(coerceVector(getListElement(Demov,TOPYNAME),REALSXP));
	ly = REAL(coerceVector(getListElement(Demov,BOTYNAME),REALSXP));
	
	L.setpoploc(e,lx,rx,uy,ly);
	L.setextinct(e,ev);
	L.setk(e,kv);
	
	///Vital Vectors: probability of observing a particular local
	///demography in a habitat.  This vector is the length of the
	///number of local demographies
	
	dv = REAL(coerceVector(getListElement(Demov,LPNAME),REALSXP));

	L.setldemovector(e,dv);

#ifdef RDEBUG
	cerr <<"Finished converting vectors to metasim for epoch: "<<e<<endl;
#endif

	///Matrices
	sz = INTEGER(coerceVector(getAttrib(getListElement(Demov,SNAME), R_DimSymbol), INTSXP))[0];
	
	for (j=0;j<sz;j++)
	  {
	    for (i=0;i<sz;i++)
	      {
		L.setSmatElement(e,i,j,REAL(coerceVector(getListElement(Demov,SNAME), REALSXP))[i+j*sz]);
		L.setRmatElement(e,i,j,REAL(coerceVector(getListElement(Demov,RNAME), REALSXP))[i+j*sz]);
		L.setMmatElement(e,i,j,REAL(coerceVector(getListElement(Demov,MNAME), REALSXP))[i+j*sz]);
	      }
	    for (k=0;k<6;k++)
	      {///may need to transpose k and j to produce correct matrix orientation
		L.setPollenkern(e,j,k,REAL(coerceVector(getListElement(Demov,PKNAME), REALSXP))[j+k*sz]);
		L.setSeedkern(e,j,k,REAL(coerceVector(getListElement(Demov,SKNAME), REALSXP))[j+k*sz]);
	      }
	  }
 	UNPROTECT(1); ///Demov
      }
#ifdef RDEBUG
    cerr << "Finished converting for all epochs"<<endl;
#endif    
    UNPROTECT(1);///Evec
     
    SEXP Ldemos = getListElement(inlist,LOCALDEMNM);
    PROTECT(Ldemos);
    for (d=0;d<ld;d++)
      {
	SEXP Lvec = VECTOR_ELT(Ldemos,d);
	PROTECT(Lvec);
	///Matrices
	sz = INTEGER(coerceVector(getAttrib(getListElement(Lvec,LCLSMATNM), R_DimSymbol), INTSXP))[0];
	for (j=0;j<sz;j++)
	  {
	    for (i=0;i<sz;i++)
	      {
		L.setLSmatElement(d,i,j,REAL(coerceVector(getListElement(Lvec,LCLSMATNM), REALSXP))[i+j*sz]);
		L.setLRmatElement(d,i,j,REAL(coerceVector(getListElement(Lvec,LCLRMATNM), REALSXP))[i+j*sz]);
		L.setLMmatElement(d,i,j,REAL(coerceVector(getListElement(Lvec,LCLMMATNM), REALSXP))[i+j*sz]);
	      }
	  }
	UNPROTECT(1);
      }
    UNPROTECT(1);
#ifdef RDEBUG
    cerr << "Finished converting for all local demographies"<<endl;
#endif    

  }

void R_to_metasim_loci(SEXP inlist, Landscape_space_statistics& L)
  {
    ///Loci:  Go through R locus object and convert to Atbls
    
    const char *ststr;
    ststr = NULL;
    int andx,i=0,j=0,sl=0;
    int nloc = length(inlist);///number of loci
    int l =0;
    int ltype;
    
    AlleleTbl *AT;
    AT =NULL;

    for (l=0; l<nloc;l++)///loop across loci
      {
	//	cerr << "converting locus "<<l<< " to landscape"<<endl;
	SEXP Locus = VECTOR_ELT(inlist,l);
	PROTECT(Locus);
	ltype = INTEGER(coerceVector(getListElement(Locus,TYPENAME),INTSXP))[0];
	if (ltype==INFALLELETBL)
	  {
	    AT = new InfAlleleTbl;
	    AT->clear();
	    int numa = length(getListElement(Locus,ALISTNAME));
	    for (i=0;i<numa;i++)
	      {
		SEXP na = VECTOR_ELT(getListElement(Locus,ALISTNAME),i);
		PROTECT(na);
		Allele ali;
		ali.SetBirth(INTEGER(coerceVector(getListElement(na,ABIRTHNAME), INTSXP))[0]);
		ali.SetProp(REAL(coerceVector(getListElement(na,PROPNAME), REALSXP))[0]);
		ali.SetState(INTEGER(coerceVector(getListElement(na,STATENAME), INTSXP))[0]);
		andx = INTEGER(coerceVector(getListElement(na,AINDXNAME), INTSXP))[0];
		
		AT->addAlleleAndIndexRef(&ali,andx);
		UNPROTECT(1);///na
	      }
	  }
	else if (ltype==STEPALLELETBL)
	  {
	    AT = new StepAlleleTbl;
	    AT->clear();
	    int numa = length(getListElement(Locus,ALISTNAME));
	    for (i=0;i<numa;i++)
	      {
		SEXP na = VECTOR_ELT(getListElement(Locus,ALISTNAME),i);
		PROTECT(na);
		Allele ali;
		ali.SetBirth(INTEGER(coerceVector(getListElement(na,ABIRTHNAME),INTSXP))[0]);
		ali.SetProp(REAL(coerceVector(getListElement(na,PROPNAME), REALSXP))[0]);
		ali.SetState(INTEGER(coerceVector(getListElement(na,STATENAME), INTSXP))[0]);
		andx = INTEGER(coerceVector(getListElement(na,AINDXNAME),INTSXP))[0];
		
		AT->addAlleleAndIndexRef(&ali,andx);
		UNPROTECT(1);///na
	      }
	  }
	else if (ltype==SEQALLELETBL)
	  {
	    AT = new SeqAlleleTbl;
	    AT->clear();
	    int numa = length(getListElement(Locus,ALISTNAME));
	    for (i=0;i<numa;i++)
	      {
		SEXP na = VECTOR_ELT(getListElement(Locus,ALISTNAME),i);
		PROTECT(na);
		
	        ststr = CHAR(asChar(getListElement(na,STATENAME)));
		sl = strlen(ststr);
		assert(sl<=MAXSEQLEN);
		assert(sl>0);
		
		SeqAllele als(sl);
		als.SetBirth(INTEGER(coerceVector(getListElement(na,ABIRTHNAME),INTSXP))[0]);
		als.SetProp(REAL(coerceVector(getListElement(na,PROPNAME),REALSXP))[0]);
		
		for (j=0;j<sl;j++)
		  {
		    als.SetSite(ststr[j],j);
		  }
		AT->setSeqLen(j);
		andx = INTEGER(coerceVector(getListElement(na,AINDXNAME),INTSXP))[0];
#ifdef RDEBUG
		//		cerr << "Allele index: "<<andx<<" Allele: ";
		//		als.Write(cerr);
#endif
		AT->addAlleleAndIndexRef(&als,andx);
		UNPROTECT(1);///na
	      }
	  }
	else
	  {
	    error("Could not identify Locus Type: %i", INTEGER(getListElement(Locus,TYPENAME))[0]);
	  }

	AT->setPloidy(INTEGER(coerceVector(getListElement(Locus,PLOIDYNAME),INTSXP))[0]);
	AT->setTrans(INTEGER(coerceVector(getListElement(Locus,TRANSNAME),INTSXP))[0]);
	AT->setMutationRate(REAL(coerceVector(getListElement(Locus,RATENAME),REALSXP))[0]);

	L.Atbl_push_back(AT);
#ifdef RDEBUG
	cerr << "this is locus "<<l<<endl;
	AT->Write(cerr);
#endif
	///	delete AT;
	UNPROTECT(1);///Locus
      }
#ifdef RDEBUG
    cerr << "The number of loci inserted was: "<< L.getloci()<<endl;
    L.WriteLoci(cerr);
#endif
  }


  void R_to_metasim_ind(SEXP inmat, Landscape_space_statistics &L)
  {
    PackedIndividual_space ind;
    int i,k,j,l;
    int nc=0;
    int nr=0;

    if (!isMatrix(inmat))
      {
	error("inmat is not a matrix in R_to_metasim_ind");
      }
    
    int *dims = INTEGER(coerceVector(getAttrib(inmat, R_DimSymbol), INTSXP));
    nr = dims[0];
    nc = dims[1];

    i=NONGENOTYPECOLS ;///number of non genotypic categories

    for (j=0;j<L.getloci();j++)
      {
	for (k=0;k<L.LocusGetPloidy(j); k++)
	  {
	    i++;
	  }
      }
    if (i!=nc)
      {
	error("converting individuals: the number and type of loci must be set before invocation");
      }

    L.reserveclasses();
    
    for (j=0;j<nr;j++)
      {
	L.SetUpInd(ind);
	i=0;
	ind.SetClass(INTEGER(coerceVector(inmat,INTSXP))[j+ i*nr]);
	i++;
	ind.SetSex(INTEGER(coerceVector(inmat, INTSXP))[j+ i*nr]);
	i++;
	ind.SetGen(INTEGER(coerceVector(inmat, INTSXP))[j+ i*nr]);
	i++;
	ind.SetX(INTEGER(inmat)[j+ i*nr]);
	i++;
	ind.SetY(INTEGER(inmat)[j+ i*nr]);
	i++;
	ind.SetMX(INTEGER(inmat)[j+ i*nr]);
	i++;
	ind.SetMY(INTEGER(inmat)[j+ i*nr]);
	i++;
	ind.SetFX(INTEGER(inmat)[j+ i*nr]);
	i++;
	ind.SetFY(INTEGER(inmat)[j+ i*nr]);
	i++;
	
	for (l=0;l<L.getloci();l++)
	  {
	    for (k=0;k<L.LocusGetPloidy(l); k++)
	      {
		ind.SetAllele(l,k,INTEGER(coerceVector(inmat, INTSXP))[j+ i*nr]);
		i++;
	      }
	  }
#ifdef RDEBUG
	cerr<<"adding an individual "<<endl;
#endif
	L.addIndividual(ind,-1);
      }
  }

void convert_R_to_metasim(SEXP Rland, Landscape_space_statistics &L)
{

    if (!isNewList(Rland))
      {
	error( "R landscape object should be a list");
      }
    R_to_metasim_ints(getListElement(Rland,INTEGERPARAMS),L);
    R_to_metasim_switches(getListElement(Rland,SWITCHPARAMS),L);
    R_to_metasim_float(getListElement(Rland,FLOATPARAMS),L);
    R_to_metasim_demography(getListElement(Rland,DEMOPARAMS),L);
    R_to_metasim_loci(getListElement(Rland,LOCIPARAMS),L);
    R_to_metasim_expression(getListElement(Rland,EXPRESSIONPARAMS),L);
    R_to_metasim_ind(getListElement(Rland,INDPARAMS),L);

}

  
SEXP write_landscape(SEXP fn, SEXP Rland)
  {
    Landscape_space_statistics L;

    ofstream OSTRM;
    OSTRM.open(CHARACTER_VALUE(fn));
    if (!OSTRM)
      {
#ifdef DEBUG
	cerr <<"fn "<<CHARACTER_VALUE(fn)<<endl;
#endif
	error ("could not open output file name:");
	return ScalarInteger(1);
      }
    convert_R_to_metasim(Rland,L);  
    OSTRM << L;
    OSTRM.close();
    return ScalarInteger(0);
  }

/* 
   
read in landscapes

 */

  SEXP metasim_to_R_ints(Landscape_space_statistics &L)
  {
    ///allocate the scalar values that describe the landscape to 'Slist'
    SEXP Slistn = PROTECT(allocVector (STRSXP,11));
    SEXP Slist = PROTECT(allocVector (VECSXP,11));

    SET_STRING_ELT(Slistn, 0, mkChar(HABNAMES    )); 
    SET_STRING_ELT(Slistn, 1, mkChar(STAGENAME   )); 
    SET_STRING_ELT(Slistn, 2, mkChar(LNUMNAME    )); 
    SET_STRING_ELT(Slistn, 3, mkChar(ENUMNAME    )); 
    SET_STRING_ELT(Slistn, 4, mkChar(CGNAME      )); 
    SET_STRING_ELT(Slistn, 5, mkChar(CENAME      )); 
    SET_STRING_ELT(Slistn, 6, mkChar(FINALAGE    )); 
    SET_STRING_ELT(Slistn, 7, mkChar(DNUMNAME    )); 
    SET_STRING_ELT(Slistn, 8, mkChar(MAXLANDNAME ));
    SET_STRING_ELT(Slistn, 9, mkChar(NPHENNAME   ));
    SET_STRING_ELT(Slistn, 10, mkChar(RDEMONAME   ));

    setAttrib(Slist, R_NamesSymbol, Slistn);

    SET_VECTOR_ELT(Slist, 0, ScalarReal(L.gethabs()));
    SET_VECTOR_ELT(Slist, 1, ScalarReal(L.getstages()));
    SET_VECTOR_ELT(Slist, 2, ScalarReal(L.getloci()));
    SET_VECTOR_ELT(Slist, 3, ScalarReal(L.getepochs()));
    SET_VECTOR_ELT(Slist, 4, ScalarReal(L.getCgen()));
    SET_VECTOR_ELT(Slist, 5, ScalarReal(L.getCepoch()));
    SET_VECTOR_ELT(Slist, 6, ScalarReal(L.getgens()));
    SET_VECTOR_ELT(Slist, 7, ScalarReal(L.getndemo()));
    SET_VECTOR_ELT(Slist, 8, ScalarReal(L.getMaxLandSize()));
    SET_VECTOR_ELT(Slist, 9, ScalarReal(L.getnphen()));
    SET_VECTOR_ELT(Slist, 10, ScalarReal(L.getranddemo()));

    UNPROTECT(2);
    return Slist;
  }

  SEXP metasim_to_R_switches(Landscape_space_statistics &L)
  {
    ///allocate the boolean switch values that describe the landscape to 'Swlist'
    SEXP Swlist = PROTECT(allocVector (VECSXP,3));
    SEXP Swlistn = PROTECT(allocVector (STRSXP,3));
    
    SET_STRING_ELT(Swlistn, 0, mkChar(RANDEPOCHN)); 
    SET_STRING_ELT(Swlistn, 1, mkChar(RANDDEMON )); 
    SET_STRING_ELT(Swlistn, 2, mkChar(MULTPNAME)); 
    
    setAttrib(Swlist, R_NamesSymbol, Swlistn);
    
    SET_VECTOR_ELT(Swlist, 0, ScalarReal(L.getrandepoch()));
    SET_VECTOR_ELT(Swlist, 1, ScalarReal(L.getranddemo()));
    SET_VECTOR_ELT(Swlist, 2, ScalarReal(L.getmultp()));
    UNPROTECT(2);
    return Swlist;  
  }

  SEXP metasim_to_R_float(Landscape_space_statistics &L)
  {  ///allocate the floating point values that describe the landscape to 'Flist'
    SEXP Flist = PROTECT(allocVector (VECSXP,13));
    SEXP Flistn = PROTECT(allocVector (STRSXP,13));
    
    SET_STRING_ELT(Flistn, 0, mkChar(SELFRATENAME)); 
    SET_STRING_ELT(Flistn, 1, mkChar(SEEDMUNAME)); 
    SET_STRING_ELT(Flistn, 2, mkChar(SEEDSHAPENAME)); 
    SET_STRING_ELT(Flistn, 3, mkChar(POLLENMUNAME)); 
    SET_STRING_ELT(Flistn, 4, mkChar(POLLENSHAPENAME)); 
    SET_STRING_ELT(Flistn, 5, mkChar(SEEDMU2NAME)); 
    SET_STRING_ELT(Flistn, 6, mkChar(SEEDSHAPE2NAME)); 
    SET_STRING_ELT(Flistn, 7, mkChar(SEEDMIXNAME)); 
    SET_STRING_ELT(Flistn, 8, mkChar(ASPECTNAME)); 

    SET_STRING_ELT(Flistn, 9, mkChar(POLLENMU2NAME)); 
    SET_STRING_ELT(Flistn, 10, mkChar(POLLENSHAPE2NAME)); 
    SET_STRING_ELT(Flistn, 11, mkChar(POLLENMIXNAME)); 
    SET_STRING_ELT(Flistn, 12, mkChar(MINDENSNAME)); 


    setAttrib(Flist, R_NamesSymbol, Flistn);
    SET_VECTOR_ELT(Flist, 0, ScalarReal(L.getself()));
    SET_VECTOR_ELT(Flist, 1, ScalarReal(L.getseed_mu()));
    SET_VECTOR_ELT(Flist, 2, ScalarReal(L.getseed_shape()));
    SET_VECTOR_ELT(Flist, 3, ScalarReal(L.getpollen_mu()));
    SET_VECTOR_ELT(Flist, 4, ScalarReal(L.getpollen_shape()));
    SET_VECTOR_ELT(Flist, 5, ScalarReal(L.getseed_mu2()));
    SET_VECTOR_ELT(Flist, 6, ScalarReal(L.getseed_shape2()));
    SET_VECTOR_ELT(Flist, 7, ScalarReal(L.getseed_mix()));
    SET_VECTOR_ELT(Flist, 8, ScalarReal(L.getaspect()));
    
    SET_VECTOR_ELT(Flist, 9, ScalarReal(L.getpollen_mu2()));
    SET_VECTOR_ELT(Flist, 10, ScalarReal(L.getpollen_shape2()));
    SET_VECTOR_ELT(Flist, 11, ScalarReal(L.getpollen_mix()));

    SET_VECTOR_ELT(Flist, 12, ScalarReal(L.getmindens()));

    
    UNPROTECT(2);
    return Flist;
  }
  
  SEXP metasim_to_R_expression(Landscape_space_statistics &L)
  {
    int i,j;
    SEXP Elist = PROTECT(allocVector (VECSXP,2));
    SEXP Elistn = PROTECT(allocVector (STRSXP,2));
    
    SET_STRING_ELT(Elistn, 0, mkChar(EXPMATNAME)); 
    SET_STRING_ELT(Elistn, 1, mkChar(HERITABLENAME)); 
    
    int np=L.getnphen();
    int nl=L.getloci();
    SEXP expmat = PROTECT(allocMatrix(REALSXP, nl, np));
    SEXP hsq = PROTECT(allocVector(REALSXP, np));

    for (i=0;i<nl;i++)
      for (j=0;j<np;j++)
	{
	  REAL(coerceVector(expmat, REALSXP))[i*np+j] = L.getexpmatel(i,j);
	}
    for (j=0;j<np;j++)
      {
	REAL(hsq)[j] = L.getheritability(j);
      }
    setAttrib(Elist, R_NamesSymbol, Elistn);
    SET_VECTOR_ELT(Elist, 0, expmat);
    SET_VECTOR_ELT(Elist, 1, hsq);

    UNPROTECT(4);
    return Elist;
  }

  SEXP metasim_to_R_demography(Landscape_space_statistics &L)
  {
    ///Demography vectors: these are lists that contain demographic
    ///parameters for simulation

    int e,i=0,j=0,k=0;
    int sz=0,d=0;
    char nbuf[64];

    SEXP LDemol = PROTECT(allocVector(VECSXP, L.getndemo()));

    sz = L.getstages();
    for (d=0;d<L.getndemo();d++)
      {
	SEXP LDemos = PROTECT(allocVector(VECSXP, 3));
	SEXP LDemosn = PROTECT(allocVector(STRSXP, 3));
	SET_STRING_ELT(LDemosn, 0, mkChar(LCLSMATNM)); 
	SET_STRING_ELT(LDemosn, 1, mkChar(LCLRMATNM)); 
	SET_STRING_ELT(LDemosn, 2, mkChar(LCLMMATNM)); 
	setAttrib(LDemos, R_NamesSymbol, LDemosn);

	SEXP LSMat = PROTECT(allocMatrix(REALSXP, sz, sz));
	SEXP LRMat = PROTECT(allocMatrix(REALSXP, sz, sz));
	SEXP LMMat = PROTECT(allocMatrix(REALSXP, sz, sz));
	for (j=0;j<sz;j++)
	  {
	    for (i=0;i<sz;i++)
	      {
		REAL(coerceVector(LSMat, REALSXP))[i+j*sz] = L.getLSmatElement(d,i,j);
		REAL(coerceVector(LRMat, REALSXP))[i+j*sz] = L.getLRmatElement(d,i,j);
		REAL(coerceVector(LMMat, REALSXP))[i+j*sz] = L.getLMmatElement(d,i,j);
	      }
	  }
#ifdef RDEBUG
	cerr <<"Setting local demos"<<endl;
#endif
	SET_VECTOR_ELT(LDemos,0,LSMat);
	SET_VECTOR_ELT(LDemos,1,LRMat);
	SET_VECTOR_ELT(LDemos,2,LMMat);
	SET_VECTOR_ELT(LDemol,d,LDemos);
	UNPROTECT(5);
      }
 
    ///Epoch vectors: these are lists nep long that contain demography lists
    ///this way demography can change in every epoch
    SEXP Epochs = PROTECT(allocVector(VECSXP, L.getepochs()));
    SEXP Epochsn = PROTECT(allocVector(STRSXP, L.getepochs()));
    

    for (e=0;e<L.getepochs();e++)  
      {
	SEXP Demov = PROTECT(allocVector(VECSXP, 14));
	SEXP Demovn = PROTECT(allocVector(STRSXP, 14));
	SET_STRING_ELT(Demovn, 0, mkChar(RNDCHSNAME )); 
	SET_STRING_ELT(Demovn, 1, mkChar(SGENAME    )); 
	SET_STRING_ELT(Demovn, 2, mkChar(EXTINCTNAME)); 
	SET_STRING_ELT(Demovn, 3, mkChar(CARRYNAME  )); 
	SET_STRING_ELT(Demovn, 4, mkChar(LPNAME     )); 
	SET_STRING_ELT(Demovn, 5, mkChar(SNAME      )); 
	SET_STRING_ELT(Demovn, 6, mkChar(RNAME      )); 
	SET_STRING_ELT(Demovn, 7, mkChar(MNAME      )); 
	SET_STRING_ELT(Demovn, 8, mkChar(LEFTXNAME  )); 
	SET_STRING_ELT(Demovn, 9, mkChar(RIGHTXNAME )); 
	SET_STRING_ELT(Demovn, 10, mkChar(TOPYNAME  )); 
	SET_STRING_ELT(Demovn, 11, mkChar(BOTYNAME  )); 
	SET_STRING_ELT(Demovn, 12, mkChar(PKNAME  )); 
	SET_STRING_ELT(Demovn, 13, mkChar(SKNAME  )); 


	setAttrib(Demov, R_NamesSymbol, Demovn);
    
	sprintf(nbuf,"%d",e);
#ifdef RDEBUG
	cerr <<"Setting epoch name: e="<<e<<endl;
	Rprintf("epoch set %d \n",e);
#endif
	SET_STRING_ELT(Epochsn,e,mkChar(nbuf));
	//	SET_STRING_ELT(Epochsn,e,ScalarString(ScalarInteger(i)));
	
	///Probabilityc of choosing an epoch
	SET_VECTOR_ELT(Demov,0,ScalarReal(L.getepochprob(e)));
	
	///Probability of choosing an epoch
	SET_VECTOR_ELT(Demov,1,ScalarInteger(L.getepochstart(e)));
	
	///Vital Vectors:  extinctions
	SEXP Evec = PROTECT(allocVector(REALSXP, L.gethabs()));
	double *ev = new double[L.gethabs()];
	L.getextinct(e,ev);
	for (i=0;i<L.gethabs();i++)
	  {
	    REAL(Evec)[i] = ev[i];
	  }
	delete [] ev;

	SET_VECTOR_ELT(Demov,2,Evec);
	
	///Vital Vectors:  carry
	SEXP Kvec = PROTECT(allocVector(REALSXP, L.gethabs()));
	int *cv = new int[L.gethabs()];
	L.getk(e,cv);
	for (i=0;i<L.gethabs();i++)
	  {
	    REAL(Kvec)[i] = cv[i];
	  }
	SET_VECTOR_ELT(Demov,3,Kvec);
	delete [] cv;

	///Vital Vectors: probability of observing a particular local
	///demography in a habitat.  This vector is the length of the
	///number of local demographies
	
	double *dv = new double[L.getndemo()];
	SEXP LDvec = PROTECT(allocVector(REALSXP, L.getndemo()));
	L.getldemovector(e,dv);
	for (i=0;i<L.getndemo();i++)
	  {
	    REAL(LDvec)[i] = dv[i];
	  }
	SET_VECTOR_ELT(Demov,4,LDvec);
	delete [] dv;

	///Vital Vectors: population spatial coordinates
	///
	SEXP  leftx  = PROTECT(allocVector(REALSXP, L.gethabs()));
	SEXP  rightx = PROTECT(allocVector(REALSXP, L.gethabs()));
	SEXP  topy   = PROTECT(allocVector(REALSXP, L.gethabs()));
	SEXP  boty   = PROTECT(allocVector(REALSXP, L.gethabs()));

	
	double *lx = new double[L.gethabs()];
	double *rx = new double[L.gethabs()];
	double *tpy = new double[L.gethabs()];
	double *bty = new double[L.gethabs()];

	L.getpoploc(e,lx,rx,tpy,bty);
	for (i=0;i<L.gethabs();i++)
	  {
	    REAL(leftx)[i]  = lx[i];
	    REAL(rightx)[i] = rx[i];
	    REAL(topy)[i]   = tpy[i];
	    REAL(boty)[i]   = bty[i];
	  }
	
	delete [] lx;
	delete [] rx;
	delete [] tpy;
	delete [] bty;

#ifdef RDEBUG
	cerr <<"Finished setting up vectors for epoch: "<<e<<endl;
#endif
	
	
	///Matrices
	sz = L.gethabs()*L.getstages();
	
	SEXP SMat = PROTECT(allocMatrix(REALSXP, sz, sz));
	SEXP RMat = PROTECT(allocMatrix(REALSXP, sz, sz));
	SEXP MMat = PROTECT(allocMatrix(REALSXP, sz, sz));

	SEXP PK   = PROTECT(allocMatrix(REALSXP, sz, 6));
	SEXP SK   = PROTECT(allocMatrix(REALSXP, sz, 6));
	for (j=0;j<sz;j++)
	  {
	    for (i=0;i<sz;i++)
	      {
		REAL(SMat)[i+j*sz] = L.getSmatElement(e,i,j);
		REAL(RMat)[i+j*sz] = L.getRmatElement(e,i,j);
		REAL(MMat)[i+j*sz] = L.getMmatElement(e,i,j);
	      }
	    for (k=0;k<6;k++)
	      {
		REAL(PK)[j+k*sz] = L.getPollenkern(e,j,k);
		REAL(SK)[j+k*sz] = L.getSeedkern(e,j,k);
	      }
	  }
	SET_VECTOR_ELT(Demov,5,SMat);
	SET_VECTOR_ELT(Demov,6,RMat);
	SET_VECTOR_ELT(Demov,7,MMat);

	SET_VECTOR_ELT(Demov,8,leftx);
	SET_VECTOR_ELT(Demov,9,rightx);
	SET_VECTOR_ELT(Demov,10,topy);
	SET_VECTOR_ELT(Demov,11,boty);
	SET_VECTOR_ELT(Demov,12,PK);
	SET_VECTOR_ELT(Demov,13,SK);

	SET_VECTOR_ELT(Epochs,e,Demov);
	UNPROTECT(14);
      }
    SEXP Demography = PROTECT(allocVector(VECSXP, 2));
    SEXP Demographyn = PROTECT(allocVector(STRSXP, 2));
    SET_STRING_ELT(Demographyn, 0, mkChar(LOCALDEMNM)); 
    SET_STRING_ELT(Demographyn, 1, mkChar(EPOCHDEMNM)); 
    setAttrib(Demography, R_NamesSymbol, Demographyn);
    SET_VECTOR_ELT(Demography,0,LDemol);
    SET_VECTOR_ELT(Demography,1,Epochs);

    UNPROTECT(5);
    return Demography;
  }
  
  SEXP metasim_to_R_loci(Landscape_space_statistics& L)
  {
    ///Loci:  Go through Atbls and produce an object for each locus in each ind.

    
    char* Seq;

    vector<int> aindx;

    SeqAllele als;
    Allele ali;
    int an=0,a,andx,i=0,j=0,sl=0;
#ifdef RDEBUG
    cerr << "converting landscape loci into R "<<endl;
#endif
    SEXP Loci = PROTECT(allocVector(VECSXP,L.getloci()));
    SEXP Allelen = PROTECT(allocVector(STRSXP, ALLELELEN));
    SEXP Locusn = PROTECT(allocVector(STRSXP,LOCUSLEN));

#ifdef RDEBUG
    cerr << "setting up names for list "<<endl;
#endif
    SET_STRING_ELT(Allelen, 0, mkChar(AINDXNAME )); 
    SET_STRING_ELT(Allelen, 1, mkChar(ABIRTHNAME)); 
    SET_STRING_ELT(Allelen, 2, mkChar(PROPNAME  )); 
    SET_STRING_ELT(Allelen, 3, mkChar(STATENAME )); 

    SET_STRING_ELT(Locusn, 0,  mkChar(TYPENAME  )); 
    SET_STRING_ELT(Locusn, 1,  mkChar(PLOIDYNAME)); 
    SET_STRING_ELT(Locusn, 2,  mkChar(TRANSNAME  )); 
    SET_STRING_ELT(Locusn, 3,  mkChar(RATENAME  )); 
    SET_STRING_ELT(Locusn, 4,  mkChar(ALISTNAME )); 
  

    i=0;
#ifdef RDEBUG
    cerr << "actually going through loci "<<endl;
#endif

    for (i=0;i<L.getloci();i++)
      {
	SEXP Locus = PROTECT(allocVector(VECSXP, LOCUSLEN));
	setAttrib(Locus,R_NamesSymbol, Locusn);
    
#ifdef RDEBUG
	cerr << "setting up characteristics for locus "<<i<<endl;
#endif
	SET_VECTOR_ELT(Locus,0,ScalarInteger(L.LocusGetClassType(i)));
	SET_VECTOR_ELT(Locus,1,ScalarInteger(L.LocusGetPloidy(i)));
	SET_VECTOR_ELT(Locus,2,ScalarInteger(L.LocusGetTrans(i)));
	SET_VECTOR_ELT(Locus,3,ScalarReal(L.LocusGetMutRate(i)));

#ifdef RDEBUG
	cerr << "done setting up characteristics for locus "<<i<<endl;
	cerr << "getting allele indices for locus "<<i<<endl;
#endif

	aindx = L.LocusGetAindices(i);
	an = aindx.size();
#ifdef RDEBUG
	cerr << "done getting allele indices for locus "<<i<<endl;
	cerr << "there are "<<an<<" allele indices for locus "<<i<<endl;
#endif
	SEXP Alist = PROTECT(allocVector(VECSXP,an));
	for (a=0;a<an;a++)
	  {
	    SEXP Allele = PROTECT(allocVector(VECSXP,ALLELELEN));
	    setAttrib(Allele,R_NamesSymbol, Allelen);	

	    andx = aindx[a];
	    SET_VECTOR_ELT(Allele,0,ScalarInteger(andx));
	    if (L.LocusGetClassType(i)==SEQALLELETBL)
	      {
		L.LocusGetAlleleRef(i,andx,&als);
		sl = als.GetSeqSize();
		Seq = new char[sl+1];
		Seq[sl] = '\0';
#ifdef RDEBUG
		cerr <<"sequence length = "<<sl<<endl;
#endif
		for (j=0;j<sl;j++)
		  {
		    Seq[j] = als.GetSite(j);
		  }
		SET_VECTOR_ELT(Allele,3,mkString(Seq));
		delete Seq;
		SET_VECTOR_ELT(Allele,1,ScalarInteger(als.GetBirth()));
		SET_VECTOR_ELT(Allele,2,ScalarReal(als.GetProp()));
	      }
	    else if (L.LocusGetClassType(i)==INFALLELETBL)
	      {
		L.LocusGetAlleleRef(i,andx,&ali);
		SET_VECTOR_ELT(Allele,1,ScalarInteger(ali.GetBirth()));
		SET_VECTOR_ELT(Allele,2,ScalarReal(ali.GetProp()));
		SET_VECTOR_ELT(Allele,3,ScalarInteger(ali.GetState()));
	      }
	    else if (L.LocusGetClassType(i)==STEPALLELETBL)
	      {
		L.LocusGetAlleleRef(i,andx,&ali);
		SET_VECTOR_ELT(Allele,1,ScalarInteger(ali.GetBirth()));
		SET_VECTOR_ELT(Allele,2,ScalarReal(ali.GetProp()));
		SET_VECTOR_ELT(Allele,3,ScalarInteger(ali.GetState()));
	      }
	    else
	      {
		error("Could not find locus type while reading loci");
	      }
	    SET_VECTOR_ELT(Alist,a,Allele);
	    UNPROTECT(1);
	  }///end iteration ove alleles
	SET_VECTOR_ELT(Locus,4,Alist);
	UNPROTECT(1);
	SET_VECTOR_ELT(Loci,i,Locus);
	UNPROTECT(1);
      }///end iteration over loci
#ifdef RDEBUG
    cerr << "finished iterating over loci "<<endl;
#endif

    UNPROTECT(3);
    return Loci;
  }


  SEXP metasim_to_R_ind(Landscape_space_statistics &L)
  {
    PackedIndividual_space ind;
    int i,k,tr;
    int j;
    int nc=0;
    int nr=0;
    int ci=0;

    nc = NONGENOTYPECOLS; ///the first 9 columns are class, sex, gen, x, and y (x,y mother; x,y father)

    for (j=0;j<L.getloci();j++)
      {
	for (i=0;i<L.LocusGetPloidy(j); i++)
	  {
	    nc++;
	  }
      }
    tr=L.PopSize();
#ifdef RDEBUG
    cerr <<"number of individuals in landscape "<< tr <<endl;
#endif
    SEXP Indmat= PROTECT(allocMatrix(INTSXP,tr,nc));
    nr=0;
    for (i=0;i<(L.getstages()*L.gethabs());i++)
      {
	L.resetStage(i);
	if (L.StageSize(i)>0)
	  {
	    do
	      {
		ind = L.getNextInd(i);
		ci=0;
		INTEGER(coerceVector(Indmat, INTSXP))[nr+ci*tr] = i;
		ci++;
		INTEGER(coerceVector(Indmat, INTSXP))[nr+ci*tr] = ind.GetSex();
		ci++;
		INTEGER(coerceVector(Indmat, INTSXP))[nr+ci*tr] = ind.GetGen();
		ci++;
		INTEGER(coerceVector(Indmat, INTSXP))[nr+ci*tr] = int(ind.GetX());
		ci++;
		INTEGER(coerceVector(Indmat, INTSXP))[nr+ci*tr] = int(ind.GetY());
		ci++;
		INTEGER(coerceVector(Indmat, INTSXP))[nr+ci*tr] = int(ind.GetMX());
		ci++;
		INTEGER(coerceVector(Indmat, INTSXP))[nr+ci*tr] = int(ind.GetMY());
		ci++;
		INTEGER(coerceVector(Indmat, INTSXP))[nr+ci*tr] = int(ind.GetFX());
		ci++;
		INTEGER(coerceVector(Indmat, INTSXP))[nr+ci*tr] = int(ind.GetFY());
		ci++;
		for (j=0;j<L.getloci();j++)
		  {
		    for (k=0;k<L.LocusGetPloidy(j); k++)
		      {
			INTEGER(coerceVector(Indmat, INTSXP))[nr+ci*tr]= ind.GetAllele(j,k);
			ci++;
		      }
		  }
		nr++;
	      }
	    while (!L.advanceStagePtr(i));
	  }
      }
    UNPROTECT(1);
    return Indmat;
  }



SEXP convert_metasim_to_R(Landscape_space_statistics &L)
{
    ///Set up the return vector 'RetList'
    ///The return list 
    SEXP Retlist = PROTECT(allocVector (VECSXP,7));
    SET_VECTOR_ELT(Retlist, 0, metasim_to_R_ints(L));
    SET_VECTOR_ELT(Retlist, 1, metasim_to_R_switches(L));
    SET_VECTOR_ELT(Retlist, 2, metasim_to_R_float(L));
    SET_VECTOR_ELT(Retlist, 3, metasim_to_R_demography(L));
    SET_VECTOR_ELT(Retlist, 4, metasim_to_R_loci(L));
    SET_VECTOR_ELT(Retlist, 5, metasim_to_R_expression(L));
    SET_VECTOR_ELT(Retlist, 6, metasim_to_R_ind(L));

    ///Names of elements in the return list
    SEXP Retlistn = PROTECT(allocVector (STRSXP,7));
    
    SET_STRING_ELT(Retlistn, 0, mkChar(INTEGERPARAMS));
    SET_STRING_ELT(Retlistn, 1, mkChar(SWITCHPARAMS));
    SET_STRING_ELT(Retlistn, 2, mkChar(FLOATPARAMS));
    SET_STRING_ELT(Retlistn, 3, mkChar(DEMOPARAMS));
    SET_STRING_ELT(Retlistn, 4, mkChar(LOCIPARAMS));
    SET_STRING_ELT(Retlistn, 5, mkChar(EXPRESSIONPARAMS));
    SET_STRING_ELT(Retlistn, 6, mkChar(INDPARAMS));
    setAttrib(Retlist, R_NamesSymbol, Retlistn);

    UNPROTECT(2);
    ///    Atbls_clear();
    return Retlist;
}

  SEXP read_landscape(SEXP fn)
  {
    Landscape_space_statistics L;
    ifstream ISTRM;
#ifdef RDEBUG
    ofstream OSTRM;
    OSTRM.open("rdebug.dat");
#endif
    ISTRM.open(CHARACTER_VALUE(fn));
    if (!ISTRM)
      {
#ifdef DEBUG
	cerr <<"fn "<<CHARACTER_VALUE(fn)<<endl;
#endif
	error ("could not open input file name:");
      }
#ifdef RDEBUG
    cerr <<"Reading landscape"<<endl;
#endif

    ISTRM >> L;
    ISTRM.close();
    
#ifdef RDEBUG
    cerr <<"Finished reading landscape"<<endl;
    cerr <<"writing a copy to rdebug.dat before any conversion to R format"<<endl;
    OSTRM << L;
    OSTRM.close();
#endif
    
    return convert_metasim_to_R(L);
  }


///Random number generation depends upon seed and RNG generator defined in the
  ///calling R enviroment
  SEXP iterate_landscape(SEXP numit, SEXP Rland, SEXP cmpress)
{
  Landscape_space_statistics L;
  int n,i=0;
  int compress;

  convert_R_to_metasim(Rland,L);

  L.ChooseEpoch();
  L.ConstructDemoMatrix();

  n = INTEGER(coerceVector(numit,INTSXP))[0];
  compress = INTEGER(coerceVector(cmpress,INTSXP))[0];

  for (i=0;i<n;i++)
    {
      if ((L.getgens()>L.getCgen())&&(L.PopSize()!=0))
	{
  	  L.Extirpate();
	  L.Survive();
  	  L.LandCarry();
  	  L.HabCarry();
	  L.Reproduce();
	  L.Advance();
	}
    }

  if (compress)
    {
      L.Survive();
    }
  L.LandCarry();
  L.HabCarry();

  /*
  //debug
  OSTRM.open("test6.dat");
  if (!OSTRM)
    {
      cerr <<"fn "<<"test.dat"<<endl;
      error ("could not open output file name:");
    }
  OSTRM << L;
  OSTRM.close();
  //end debug
  */

  return convert_metasim_to_R(L);
}

///Random number generation depends upon seed and RNG generator defined in the
  ///calling R enviroment
  SEXP iterate_landscape_stg0(SEXP numit, SEXP Rland, SEXP cmpress)
  {
    Landscape_space_statistics L;
    int n,i=0;
    int compress;
    
    convert_R_to_metasim(Rland,L);
    
    L.ChooseEpoch();
    L.ConstructDemoMatrix();
    
    n = INTEGER(coerceVector(numit,INTSXP))[0];
    compress = INTEGER(coerceVector(cmpress,INTSXP))[0];
    
    for (i=0;i<n;i++)
      {
	if ((L.getgens()>L.getCgen())&&(L.PopSize()!=0))
	  {
	    L.Extirpate();
	    L.Survive();
	    //	    L.HabCarry();
	    L.HabCarry_stg0();
	    L.LandCarry();

	    L.Reproduce();
	    L.Advance();
	  }
      }
    
    if (compress)
      {
	L.Survive();
      }
    L.LandCarry();
    L.HabCarry();
    
    /*
  //debug
  OSTRM.open("test6.dat");
  if (!OSTRM)
  {
  cerr <<"fn "<<"test.dat"<<endl;
  error ("could not open output file name:");
  }
  OSTRM << L;
  OSTRM.close();
  //end debug
  */

    return convert_metasim_to_R(L);
  }


///perform survival step on the landscape
SEXP survive_landscape(SEXP Rland)
{
  Landscape_space_statistics L;

  convert_R_to_metasim(Rland,L);

  L.ChooseEpoch();
  L.ConstructDemoMatrix();


  if ((L.getgens()>L.getCgen())&&(L.PopSize()!=0))
    {
      L.Survive();
    }
    
  return convert_metasim_to_R(L);
}

///perform reproduce step on the landscape
SEXP reproduce_landscape(SEXP Rland)
{
  Landscape_space_statistics L;

  convert_R_to_metasim(Rland,L);

  L.ChooseEpoch();
  L.ConstructDemoMatrix();

  if ((L.getgens()>L.getCgen())&&(L.PopSize()!=0))
    {
      L.Reproduce();
    }

  return convert_metasim_to_R(L);
}

///perform carry step on the landscape
SEXP carry_landscape(SEXP Rland)
{
  Landscape_space_statistics L;

  convert_R_to_metasim(Rland,L);

  L.ChooseEpoch();
  L.ConstructDemoMatrix();

  L.LandCarry();
  L.HabCarry();


  return convert_metasim_to_R(L);
}

///perform extinct step on the landscape
SEXP extinct_landscape(SEXP Rland)
{
  Landscape_space_statistics L;

  convert_R_to_metasim(Rland,L);

  L.ChooseEpoch();
  L.ConstructDemoMatrix();

  if ((L.getgens()>L.getCgen())&&(L.PopSize()!=0))
    {
      L.Extirpate();
    }

  return convert_metasim_to_R(L);
}

  //advance landscape
SEXP advance_landscape(SEXP Rland)
{
  Landscape_space_statistics L;

  convert_R_to_metasim(Rland,L);

  L.ChooseEpoch();
  L.ConstructDemoMatrix();

  L.Advance();

  return convert_metasim_to_R(L);
}


vector<int> sexp_int_to_vector(SEXP thelist)
{
  vector<int> retval;
  int i;
  retval.resize(length(thelist));

  for (i = 0; i<length(thelist); i++)
    {
      retval[i] = INTEGER(coerceVector(thelist,INTSXP))[i];
    }
  
  return retval;    
}

SEXP clean_landscape(SEXP Rland)
  {
    Landscape_space_statistics L;
    convert_R_to_metasim(Rland,L);  
    
    return convert_metasim_to_R(L);
  }

SEXP populate_Rland(SEXP Rland, SEXP Population_sizes)
  {
    Landscape_space_statistics L;
    vector<int> ps;

    if (!isNewList(Rland))
    {
      error( "R landscape object should be a list");
    }
    R_to_metasim_ints(getListElement(Rland,INTEGERPARAMS),L);
    R_to_metasim_switches(getListElement(Rland,SWITCHPARAMS),L);
    R_to_metasim_float(getListElement(Rland,FLOATPARAMS),L);
    R_to_metasim_demography(getListElement(Rland,DEMOPARAMS),L);
    R_to_metasim_loci(getListElement(Rland,LOCIPARAMS),L);
    R_to_metasim_expression(getListElement(Rland,EXPRESSIONPARAMS),L);
    ps = sexp_int_to_vector(Population_sizes);
    L.popsizeset(ps);

    return convert_metasim_to_R(L);
    return 0;
  }


SEXP l2w(SEXP Rland, SEXP numind)
{
  vector <int> inmat;
  Landscape_space_statistics L;
  int i, l, n;
  n = INTEGER(coerceVector(numind,INTSXP))[0];
  convert_R_to_metasim(Rland,L);
  inmat=L.Rmat(n);

  l=inmat.size();
  SEXP retvec= PROTECT(allocVector(INTSXP,l));
  
  for (i=0; i<l; i++)
    {
      INTEGER(retvec)[i]=inmat[i];
    }
  UNPROTECT(1);
  return retvec;
}

  SEXP num_demo_cols()
  {
    return ScalarInteger(NONGENOTYPECOLS);
  }

SEXP test()
  {
    SEXP Indmat= PROTECT(allocVector(REALSXP,10000));
    int i;
    double x,y;
    for (i=0; i<10000;i++)
      {
	RandLibObj.rassym_mixed_xy(1000,1000,100,200,100,200,1,50,1,50,0.5,0.5,1,x,y);
	REAL(Indmat)[i]=pow(pow(1000-x,2)+pow(1000-y,2),0.5);
      }
    UNPROTECT(1);
    return Indmat;
  }

/*
Functions that produce text files for input into other programs. 
*/

SEXP writeGDA(SEXP fn, SEXP Rland, SEXP ni)
{
  Landscape_space_statistics L;
  ofstream OSTRM;
    OSTRM.open(CHARACTER_VALUE(fn));
    if (!OSTRM)
      {
#ifdef DEBUG
	cerr <<"fn "<<CHARACTER_VALUE(fn)<<endl;
#endif
	error ("could not open output file name:");
	return ScalarInteger(1);
      }
    convert_R_to_metasim(Rland,L);  
    L.GdaOut(INTEGER(coerceVector(ni,INTSXP))[0], OSTRM);
    OSTRM.close();
    return ScalarInteger(0);
} 

SEXP writeArlequinHap(SEXP fn, SEXP Rland, SEXP ni)
{
  Landscape_space_statistics L;
  ofstream OSTRM;
  OSTRM.open(CHARACTER_VALUE(fn));
  if (!OSTRM)
    {
#ifdef DEBUG
      cerr <<"fn "<<CHARACTER_VALUE(fn)<<endl;
#endif
      error ("could not open output file name:");
      return ScalarInteger(1);
    }
  convert_R_to_metasim(Rland,L);  
  L.ArlequinHaploidOut(INTEGER(coerceVector(ni,INTSXP))[0], OSTRM);
  OSTRM.close();
  return ScalarInteger(0);
} 

SEXP writeArlequinDip(SEXP fn, SEXP Rland, SEXP ni)
{
  Landscape_space_statistics L;
    ofstream OSTRM;
    OSTRM.open(CHARACTER_VALUE(fn));
    if (!OSTRM)
      {
#ifdef DEBUG
	cerr <<"fn "<<CHARACTER_VALUE(fn)<<endl;
#endif
	error ("could not open output file name:");
	return ScalarInteger(1);
      }
    convert_R_to_metasim(Rland,L);  
    L.ArlequinDiploidOut(INTEGER(coerceVector(ni,INTSXP))[0], OSTRM);
    OSTRM.close();
    return ScalarInteger(0);
} 

SEXP writeBIOSYS(SEXP fn, SEXP Rland, SEXP ni)
{
  Landscape_space_statistics L;
    ofstream OSTRM;
    OSTRM.open(CHARACTER_VALUE(fn));
    if (!OSTRM)
      {
#ifdef DEBUG
	cerr <<"fn "<<CHARACTER_VALUE(fn)<<endl;
#endif
	error ("could not open output file name:");
	return ScalarInteger(1);
      }
    convert_R_to_metasim(Rland,L);  
    L.BiosysDiploidOut(INTEGER(coerceVector(ni,INTSXP))[0], OSTRM);
    OSTRM.close();
    return ScalarInteger(0);
} 

SEXP writeGenPop(SEXP fn, SEXP Rland, SEXP ni)
{
  Landscape_space_statistics L;
    ofstream OSTRM;
    OSTRM.open(CHARACTER_VALUE(fn));
    if (!OSTRM)
      {
#ifdef DEBUG
	cerr <<"fn "<<CHARACTER_VALUE(fn)<<endl;
#endif
	error ("could not open output file name:");
	return ScalarInteger(1);
      }
    convert_R_to_metasim(Rland,L);  
    L.GenepopOut(INTEGER(coerceVector(ni,INTSXP))[0], OSTRM);
    OSTRM.close();
    return ScalarInteger(0);
} 

SEXP writeReRat(SEXP fn, SEXP Rland, SEXP ni)
{
  Landscape_space_statistics L;
    ofstream OSTRM;
    OSTRM.open(CHARACTER_VALUE(fn));
    if (!OSTRM)
      {
#ifdef DEBUG
	cerr <<"fn "<<CHARACTER_VALUE(fn)<<endl;
#endif
	error ("could not open output file name:");
	return ScalarInteger(1);
      }
    convert_R_to_metasim(Rland,L);  
    L.MicroRatOut(INTEGER(coerceVector(ni,INTSXP))[0], OSTRM);
    OSTRM.close();
    return ScalarInteger(0);
} 

SEXP writeMigrateDip(SEXP fn, SEXP Rland, SEXP ni)
{
  Landscape_space_statistics L;
    ofstream OSTRM;
    OSTRM.open(CHARACTER_VALUE(fn));
    if (!OSTRM)
      {
#ifdef DEBUG
	cerr <<"fn "<<CHARACTER_VALUE(fn)<<endl;
#endif
	error ("could not open output file name:");
	return ScalarInteger(1);
      }
    convert_R_to_metasim(Rland,L);  
    L.MigrateDiploidOut(INTEGER(coerceVector(ni,INTSXP))[0], OSTRM);
    OSTRM.close();
    return ScalarInteger(0);
} 

SEXP writeR(SEXP fn, SEXP Rland, SEXP ni)
{
  Landscape_space_statistics L;
    ofstream OSTRM;
    OSTRM.open(CHARACTER_VALUE(fn));
    if (!OSTRM)
      {
#ifdef DEBUG
	cerr <<"fn "<<CHARACTER_VALUE(fn)<<endl;
#endif
	error ("could not open output file name:");
	return ScalarInteger(1);
      }
    convert_R_to_metasim(Rland,L);  
    L.ROut(INTEGER(coerceVector(ni,INTSXP))[0], OSTRM);
    OSTRM.close();
    return ScalarInteger(0);
} 


} ///end of extern "C"
