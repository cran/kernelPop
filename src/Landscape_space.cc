/*

$Modified: astrand $

Copyright (C) 1999 Allan E. Strand

This file is part of Metasim
*/

/*includes
*/

#include <Landscape_space.h>
#include <sstream>
#include <unistd.h>

using namespace std;


///LocalMat class

void LocalMat::SetSize(size_t sz)
{
  assert(sz>=0);

#ifdef RDEBUG
  cerr << "SetSize Slocal" <<endl;
#endif

  Slocal.SetSize(sz);

#ifdef RDEBUG
  cerr << "SetSize Rlocal" <<endl;
#endif

  Rlocal.SetSize(sz);

#ifdef RDEBUG
  cerr << "SetSize Mlocal" <<endl;
#endif

  Mlocal.SetSize(sz);
}



ostream &operator<<(ostream & stream, LocalMat &lm)
{
  stream << lm.Slocal << endl << lm.Rlocal << endl << lm.Mlocal << endl;
  return stream;
}
istream &operator>>(istream & stream, LocalMat &lm)
{
  stream >> lm.Slocal ;
  stream >> lm.Rlocal ;
  stream >> lm.Mlocal ;
  return stream;
}




///end LocalMat class


///Landscape_space class

///Constructor
Landscape_space::Landscape_space (int h, int stg, int loc, int ep, int nd, int gn)
{
  ndemo=1;
#ifdef RDEBUG
  cerr << "Constructing landscape object" <<endl;
#endif
  ///  init(h, stg, loc, ep, nd, gn);
}

///Destructor

Landscape_space::~Landscape_space()
{
#ifdef RDEBUG
  cerr << "Destructing landscape object" <<endl;
#endif
  /*
  for (int i=0;i < s*nhab; i++)
    {
      I[i].ClearClass(0);
    }


#ifdef RDEBUG
	  cerr << "Landscape_space destructor: Deleting vectors and mats i= "<<i <<endl;
#endif
	  S.resize(0);
	  R.resize(0);
	  M.resize(0);
	  for (i=0;i<nep;i++)
	    {
	      demoProbVec[i].resize(0);
	    }
	  evec.resize(0);
	  kvec.resize(0);
	  LM.resize(0);
	  demoProbVec.resize(0);
  */
#ifdef RDEBUG
  cerr << "Landscape_space destructor exiting" <<endl;
#endif

}

/**
   set the appropriate matrices
*/


void Landscape_space::setepochs(int ep)
{
  int i,m;
  
  nep=ep;
  epochs.resize(nep);
  epochprobs.resize(nep);
  S.resize(nep);
  R.resize(nep);
  M.resize(nep);
  evec.resize(nep);
  kvec.resize(nep);

  popleftx.resize(nep);
  poprightx.resize(nep);
  poptopy.resize(nep);
  popboty.resize(nep);
  PK.resize(nep);
  SK.resize(nep);
  demoProbVec.resize(nep);
  for (i=0;i<nep;i++)
    {
      S[i].SetSize(s*nhab)      ;
      R[i].SetSize(s*nhab)      ;
      M[i].SetSize(s*nhab)      ;
      if (int(demoProbVec[i].size())!=ndemo)
	{
#ifdef RDEBUG
	  cerr << "sizing i of nep demoProbVec[i] to ndemo.  i="<<i<<" ndemo="<<ndemo <<endl;
#endif
	  demoProbVec[i].resize(ndemo);
	}
#ifdef RDEBUG
  cerr << "done sizing the demoProbVecs" <<endl;
#endif
      evec[i].resize(nhab);
      kvec[i].resize(nhab);

      popleftx[i].resize(nhab);
      poprightx[i].resize(nhab);
      poptopy[i].resize(nhab);
      popboty[i].resize(nhab);
      PK[i].resize(nhab*s);
      SK[i].resize(nhab*s);
      for (m=0;m<(nhab*s);m++)
	{
	  PK[i][m].resize(6);
	  SK[i][m].resize(6);
	}
    }
}

void Landscape_space::setexpression()
{
  expmat.resize(getloci());
  hsq.resize(nphen);
  for(int i=0;i<getloci();i++)
    {
      expmat[i].resize(getnphen());
    }
}

void Landscape_space::setndemo(int nd)
{
  int i;
  ndemo=nd;
#ifdef RDEBUG
  cerr << "REserving space for LM" <<endl;
#endif
  LM.resize(ndemo);
#ifdef RDEBUG
  cerr << "done REserving space for LM" <<endl;
#endif

#ifdef RDEBUG
  cerr << "Setting sizes of LM atrices to: "<<s <<endl;
#endif
  for (i=0;i<ndemo;i++)
    {
      LM[i].SetSize(s);
    }
#ifdef RDEBUG
  cerr << "Resizing the demoProbVecs to ndemo="<<ndemo <<endl;
#endif
  for (i=0;i<nep;i++)
    {
      if (int(demoProbVec[i].size())!=ndemo)
	{
	  demoProbVec[i].resize(ndemo);
	}
    }
#ifdef RDEBUG
  cerr << "About to exit setndemo" <<endl;
#endif

}


void Landscape_space::sethabs(int h) 
{
  nhab=h;
  /*
  if ((h*s)==S[0].Size())  ///there should always be an epoch 0
    {
      nhab=h;
    }
  else
    {
      nhab=h;
#ifdef RDEBUG
      cerr << "sethabs: resetting the transition matrices" <<endl;
#endif
      
    }
  */
}

void Landscape_space::setstages(int stg) { s=stg; }
void Landscape_space::setepochprob(int ce, double prob) {epochprobs[ce]=prob;}
void Landscape_space::setepochstart(int ce, int strt) {epochs[ce]=strt;}

void Landscape_space::init(int h, int stg, int loc, int ep, int nd, int gn)
{
#ifdef RDEBUG
  cerr << "waiting for return to cont";
  cerr << endl;
  cerr << "Running: sethabs(h)" <<endl;
#endif
  sethabs(h);

#ifdef RDEBUG
  cerr << "Running: setstages(stg)" <<endl;
#endif
  setstages(stg);
#ifdef RDEBUG
  cerr << "Running: setepochs(ep)" <<endl;
#endif
  setepochs(ep);
#ifdef RDEBUG
  cerr << "Running: setndemo(nd)" <<endl;
#endif
  setndemo(nd);
#ifdef RDEBUG
  cerr << "Running: setgens(gn)" <<endl;
#endif
  setgens(gn);
#ifdef RDEBUG
  cerr << "Running: setMaxLandSize()" <<endl;
#endif
  setMaxLandSize();
#ifdef RDEBUG
  cerr << "Running: setself()" <<endl;
#endif
  setself();
  setseed_mu(1,1);
  setseed_shape(1,1);
  setseed_mix(1);
  setpollen_mu(1);
  setpollen_shape(1);

#ifdef RDEBUG
  cerr << "Running: unsetRandEpoch()" <<endl;
#endif

  unsetRandEpoch();

#ifdef RDEBUG
  cerr << "done running: unsetRandEpoch()" <<endl;
#endif
  t=0;
  e=0;

  title = "";

#ifdef RDEBUG
  cerr << "Running:   I.resize(s * nhab)" <<endl;
#endif
  I.resize(s * nhab);

  multiple_paternity = 1 ;

  ///HABDELTA is precision when inplementing carrying capacity via Carry()
  habdelta = HABDELTA;
#ifdef RDEBUG
  cerr << "end of init" <<endl;
#endif

}

void Landscape_space::setS(TransMat a, int ep)
{
  int q;
  if (ep<0) 
    {
      for (q=0;q<nep;q++)
	{
	  S[q].SetElement(0,0, 0.4); S[q].SetElement(1,0, 0.0); S[q].SetElement(2,0, 0.0); S[q].SetElement(3,0, 0.0);
	  S[q].SetElement(0,1, 0.3); S[q].SetElement(1,1, 0.6); S[q].SetElement(2,1, 0.0); S[q].SetElement(3,1, 0.2);
	  S[q].SetElement(0,2, 0.0); S[q].SetElement(1,2, 0.0); S[q].SetElement(2,2, 0.4); S[q].SetElement(3,2, 0.0);
	  S[q].SetElement(0,3, 0.0); S[q].SetElement(1,3, 0.1); S[q].SetElement(2,3, 0.3); S[q].SetElement(3,3, 0.6);
	}
    }
  else
    {
      S[ep].SetMat(a);
    }
}

void Landscape_space::setR(TransMat a, int ep)
{
  int q;
  if (ep<0) 
    {
      for (q=0;q<nep;q++)
	{
      R[q].SetElement(0,0, 0.0); R[q].SetElement(1,0, 5.0); R[q].SetElement(2,0, 0.0); R[q].SetElement(3,0, 0.0);
      R[q].SetElement(0,1, 0.0); R[q].SetElement(1,1, 0.0); R[q].SetElement(2,1, 0.0); R[q].SetElement(3,1, 0.0);
      R[q].SetElement(0,2, 0.0); R[q].SetElement(1,2, 0.0); R[q].SetElement(2,2, 0.0); R[q].SetElement(3,2, 3.0);
      R[q].SetElement(0,3, 0.0); R[q].SetElement(1,3, 0.0); R[q].SetElement(2,3, 0.0); R[q].SetElement(3,3, 0.0);
	}
    }
  else
    {
      R[ep].SetMat(a);
    }
}

void Landscape_space::setM(TransMat a, int ep)
{
  int q;
  if (ep<0) 
    {
      for (q=0;q<nep;q++)
	{
      M[q].SetElement(0,0, 0.0); M[q].SetElement(1,0, 5.0); M[q].SetElement(2,0, 0.0); M[q].SetElement(3,0, 0.0);
      M[q].SetElement(0,1, 0.0); M[q].SetElement(1,1, 0.0); M[q].SetElement(2,1, 0.0); M[q].SetElement(3,1, 0.0);
      M[q].SetElement(0,2, 0.0); M[q].SetElement(1,2, 0.0); M[q].SetElement(2,2, 0.0); M[q].SetElement(3,2, 3.0);
      M[q].SetElement(0,3, 0.0); M[q].SetElement(1,3, 0.0); M[q].SetElement(2,3, 0.0); M[q].SetElement(3,3, 0.0);
	}
    }
  else
    {
      M[ep].SetMat(a);
    }
}


void Landscape_space::setextinct(int ep, double *ev)
{
  int i;
    for (i=0;i<nhab;i++)
    {
      evec[ep][i]=ev[i];
    }
}

void Landscape_space::setk(int ep, int *cv)
{
  int i;
    for (i=0;i<nhab;i++)
    {
      kvec[ep][i]=cv[i];
    }
}


void Landscape_space::setpoploc(int ep, double *lx, double *rx,double *topy, double *boty)
{
  int i;
    for (i=0;i<nhab;i++)
    {
      popleftx[ep][i]=lx[i];
      poprightx[ep][i]=rx[i];
      poptopy[ep][i]=topy[i];
      popboty[ep][i]=boty[i];
    }
}

void Landscape_space::getextinct(int ep, double *ev)
{
  int i;
    for (i=0;i<nhab;i++)
    {
      ev[i]=evec[ep][i];
    }
}
 
void Landscape_space::getk(int ep, int *cv)
{
  int i;
    for (i=0;i<nhab;i++)
    {
      cv[i]=kvec[ep][i];
    }
}


void Landscape_space::getpoploc(int ep, double *lx, double *rx,double *topy, double *boty)
{
  int i;
    for (i=0;i<nhab;i++)
    {
      lx[i]=popleftx[ep][i];
      rx[i]=poprightx[ep][i];
      topy[i]=poptopy[ep][i];
      boty[i]=popboty[ep][i];
    }
}



void Landscape_space::setldemovector(int ep, double *dv)
{
  int i;
  for (i=0;i<ndemo;i++)
    {
      demoProbVec[ep][i]=dv[i];
    }
}

void Landscape_space::getldemovector(int ep, double *dv)
{
  int i;
    for (i=0;i<ndemo;i++)
    {
      dv[i]=demoProbVec[ep][i];
    }
}

void Landscape_space::zeroextinct()
{
  int q,i;
  for (q=0;q<nep;q++)
    for (i=0;i<nhab;i++)
    {
      evec[q][i]=0;
    }
}
 
void Landscape_space::zerok()
{
  int q,i;
  for (q=0;q<nep;q++)
    for (i=0;i<nhab;i++)
    {
      kvec[q][i]=0;
    }
}

void Landscape_space::ChooseEpoch()
{
  int i;
  if (randepoch)
    {
      RandomlyChooseEpoch();
    }
  else
    {
      for (i=0;i<nep;i++)
	{
	  if (epochs[i]<=t) 
	    {
	      e=i;
	    }
	}
    }
}

void Landscape_space::RandomlyChooseEpoch()
 {
   int i;
   double *p = new double[nep];
   if (randepoch>0)
    {
      for (i=0;i<nep;i++)
	{
	  p[i]=epochprobs[i];
	}
      e = RandLibObj.multinomial(p,nep);
    }
   delete [] p;
}

void Landscape_space::SequentiallyConstructDemoMatrix()
{

  int fr,to;
  int i,rm;
  int newto,newfr;

  rm=0;
  for (i=0;i<nhab;i++)
    {
      if (rm>=ndemo)
	{
	  rm=0;
	}
      for (fr=0;fr<s;fr++)
	{
	  for (to=0;to<s;to++)
	    {
	      newto = (s*i)+to ;
	      newfr = (s*i)+fr ;
	      S[e].SetElement(newfr,newto,LM[rm].GetSlocalVal(fr,to));
	      R[e].SetElement(newfr,newto,LM[rm].GetRlocalVal(fr,to));
	      M[e].SetElement(newfr,newto,LM[rm].GetMlocalVal(fr,to));
	    }
	}
      rm++;
    }
}
void Landscape_space::RandomlyConstructDemoMatrix()
{

  double *p = new double[ndemo];
  int fr,to;
  int i,rm;
  int newto,newfr;

  //set the probs of the multinomial distribution to pass to the rng
  for (i=0;i<ndemo;i++)
    {
      p[i]=demoProbVec[e][i];
    }
  
  for (i=0;i<nhab;i++)
    {
      rm = RandLibObj.multinomial(p,ndemo);
      for (fr=0;fr<s;fr++)
	{
	  for (to=0;to<s;to++)
	    {
	      newto = (s*i)+to ;
	      newfr = (s*i)+fr ;
	      S[e].SetElement(newfr,newto,LM[rm].GetSlocalVal(fr,to));
	      R[e].SetElement(newfr,newto,LM[rm].GetRlocalVal(fr,to));
	      M[e].SetElement(newfr,newto,LM[rm].GetMlocalVal(fr,to));
	    }
	}
    }
  delete [] p;
}

void Landscape_space::popsizeset(std::vector<int> &ps)
{
  int i,j,psz;
  int totpop ;
  PackedIndividual_space Ind;
  DemoClass_space DC;

  psz=ps.size();

  totpop = 0;

  for (i=0; i<psz; i++)
    {
      totpop = totpop + ps[i];
      I.push_back(DC);
    }

  for (i=0; i<psz; i++)
    {
      I[i].SetClass(i);
      for (j=0; j<ps[i]; j++)
	{
	  Ind.SetClass(i);
	  Ind.SetLoci(Atbls);
	  Ind.SetRandGenotype(Atbls);
	  Ind.Change(-1);
	  Ind.SetLastRep(-1);
	  Ind.SetNumOff(0);
	  Ind.Birth(-1,Atbls);

	  Ind.SetX(RandLibObj.uniminmax(int(popleftx[e][Habitat(i)]),int(poprightx[e][Habitat(i)])));
	  Ind.SetY(RandLibObj.uniminmax(int(popboty[e][Habitat(i)]),int(poptopy[e][Habitat(i)])));
	  Ind.SetMX(0);
	  Ind.SetMY(0);
	  Ind.SetFX(0);
	  Ind.SetFY(0);
	  I[i].AddIndividual(Ind);
	}
    }
}


int Landscape_space::Habitat(int stage)
{
  int retv;
  double st;
  double ns;

  st = double(stage)*1.0;
  ns = double(s)*1.0;
  retv = int(floor(st/ns));
  return retv;
}


int Landscape_space::PopSize(int p)
{
  int i,tot;
  int sz = nhab*s;

  tot=0;

  if (p!=-1)
    {
      for (i=0;i<sz;i++)
	{
	  if (Habitat(i)==p)
	    {
	      tot = tot + I[i].size();
	    }
	}
    }
  else
    {
      for (i=0;i<sz;i++)
	{
	  tot = tot + I[i].size();
	}
    }
  return tot;
}



void Landscape_space::Survive()
{
  PackedIndividual_space ind,tmpind;
  vector < int > deadindices, changeindices;
  vector < int >::iterator inditer;
  int indx;
  int rs;
  size_t i, j, isz, sz;


  deadindices.reserve(1000);
  changeindices.reserve(1000);
  sz = nhab * s;
  for (i=0;i<sz;i++)
    {
      S[e].SetFromState(i); //choose a column in the survival/migration matrix
      S[e].SetRandomToStateVec();
      I[i].ResetIndividuals();
      isz = I[i].size();
      I[i].ResetIndividuals();
      for (j=0;j<isz;j++)
	{
	  ind = I[i].GetCurrentIndividual();
	  indx = I[i].GetCurrentIndex();
	  if ((indx<0)||(ind.GetClass()<0))
	    {
#ifdef DEBUG
	      cerr << " run off the the end of the individual map for class " << i<<endl;
#endif
	      assert(ind.GetClass()>=0);
	    }
	  if (ind.GetChanged()<t)
	    {
	      rs = S[e].RandomState();
	      if (rs<0)//ind dies
		{
		  deadindices.push_back(indx);
		}
	      else if (rs!=int(i))
		{
		  ind.Change(t);
		  ind.SetClass(rs);
		  ind.Growth(Atbls);
		  I[rs].AddIndividual(ind);
		  changeindices.push_back(indx);
		}
	      else
		{
		  I[i].ChangeInd(indx,t);
		}
	    }
	  else
	    {
	    }
	  if (I[i].NextIndividual()) //advance the individual pointer
	    {
	      break;
	    }
	}

      for (inditer=deadindices.begin();inditer!=deadindices.end();inditer++)
	{
	  I[i].RemoveInd(*inditer,t,Atbls);
	}
      for (inditer=changeindices.begin();inditer!=changeindices.end();inditer++)
	{
	  I[i].RemoveInd(*inditer,t,Atbls);
	}

      deadindices.clear();
      changeindices.clear();
      RandLibObj.FreeDiscreteLookup();
    }
}

///Survive step with negative density dependence modeled as a decline in seedling survival
///that declines as an negative exponential from the maternal plant;  Effect ignores other plants
void Landscape_space::SurviveNDD()
{
  PackedIndividual_space ind,tmpind;
  vector < int > deadindices, changeindices;
  vector < int >::iterator inditer;
  int indx;
  int rs;
  size_t i, j, isz, sz;
  double normalize = 1/RandLibObj.negexp(0,ndd);

  //    cerr << "running SurviveNDD(), nddmag = "<<nddmag<<", ndd = "<<ndd<<", normalize = "<<normalize<<endl;
  
  deadindices.reserve(1000);
  changeindices.reserve(1000);
  sz = nhab * s;
  for (i=0;i<sz;i++)
    {
      S[e].SetFromState(i); //choose a column in the survival/migration matrix
      if ((i % s)!=0) {//not seedlings so set transitions probs unrelated to dist
	S[e].SetRandomToStateVec();
	//	cerr << "not seedling" << i <<endl;
      } 
      I[i].ResetIndividuals();
      isz = I[i].size();
      I[i].ResetIndividuals();
      for (j=0;j<isz;j++)
	{
	  ind = I[i].GetCurrentIndividual();
	  indx = I[i].GetCurrentIndex();
	  if ((indx<0)||(ind.GetClass()<0))
	    {
#ifdef DEBUG
	      cerr << " run off the the end of the individual map for class " << i<<endl;
#endif
	      assert(ind.GetClass()>=0);
	    }


	  if (ind.GetChanged()<t)
	    {
	      
	      if ((i % s)==0) {
		if (nddmag>0) 
		  {
		    //		    cerr<<"is seedling "<<i<<endl;
		    S[e].SetRandomToStateVec(nddmag*RandLibObj.negexp(ind.GetSeedDist(),ndd));
		    //		    cerr <<"ind x,y "<<ind.GetX()<<", "<<ind.GetY()<<" mx, my "<<ind.GetMX()<<", "<<ind.GetMY()<<endl;
		    //		    cerr<<"Dist "<<ind.GetSeedDist()<<" ndd "<<ndd<<" dens "<<RandLibObj.negexp(ind.GetSeedDist(),ndd)<<endl;
		  } else 
		  {
		    S[e].SetRandomToStateVec();
		  }
	      }

	      rs = S[e].RandomState();
	      if (rs<0)//ind dies
		{
		  deadindices.push_back(indx);
		}
	      else if (rs!=int(i))
		{
		  ind.Change(t);
		  ind.SetClass(rs);
		  ind.Growth(Atbls);
		  I[rs].AddIndividual(ind);
		  changeindices.push_back(indx);
		}
	      else
		{
		  I[i].ChangeInd(indx,t);
		}
	    }
	  else
	    {
	    }
	  if (I[i].NextIndividual()) //advance the individual pointer
	    {
	      break;
	    }
	}

      for (inditer=deadindices.begin();inditer!=deadindices.end();inditer++)
	{
	  I[i].RemoveInd(*inditer,t,Atbls);
	}
      for (inditer=changeindices.begin();inditer!=changeindices.end();inditer++)
	{
	  I[i].RemoveInd(*inditer,t,Atbls);
	}

      deadindices.clear();
      changeindices.clear();
      RandLibObj.FreeDiscreteLookup();
    }
}



/** 


This method returns a vector of males and initializes a vector of probs of choosing each male as a father

*/
vector< PackedIndividual_space >  Landscape_space::CalculateMaleGameteClassVector(PackedIndividual_space pi)
{
  int i,sz;
  size_t j;
  int dclass = nhab * s;

  vector< PackedIndividual_space > males;
  vector< PackedIndividual_space > lmales;

  double dist, tot, pkd;

  sz=0;
  M[e].SetToState(pi.GetClass());
  ///get the total number of individuals in classes that can contribute male gametes

  for (i=0;i<dclass;i++)
    { 
      M[e].SetFromState(i);
      if (M[e].Value()>0)
	{
	  sz += I[i].size();
	}
    }

  //  double p[sz];
  double *p = new double[sz+1]; //the sz+1 is for cases when there are no males. The gnu-specific 
  //contruction above dows not bomb when this happens, but the standard C++ method of declaring
  //a dynamic vector does.
  males.reserve(sz);
  lmales.reserve(sz);
  sz=0;
  tot=0;
  
  ///cout << ".";

  for (i=0;i<dclass;i++)
    { 
      M[e].SetFromState(i);
      if (M[e].Value()>0)
	{
	  males=I[i].ReturnAsVector();
	  for (j=0;j<males.size();j++)
	    {
	      //	      if (Habitat(i)==Habitat(pi.GetClass())) ///male is in same habitat as female, base prob on distance
	      //		{
		  dist = pow(pow((pi.GetX()-(males[j]).GetX()),2)+pow((pi.GetY()-(males[j]).GetY()),2),0.5);
		  if (dist>0 && dist<6000)
		    {
		      pkd = pollenKernelDensity(dist,i);
		    } 
		  else 
		    {
		      pkd=0.0;
		    }
		  if (pkd<mindens)  pkd=0.0;
		  p[sz]=M[e].Value() * pkd;
		  tot = tot + p[sz];
		  sz++;
	    }
	  //	  cerr << endl;
	  for (size_t r1=0; r1 < males.size(); r1++)
	    {
	      lmales.push_back(males[r1]);
	    }
	}
    }
  //  cerr<<"this is tot "<<tot <<endl<<"This is sz "<<sz<<endl;
  if (tot>0)
    {
      for (i=0;i<sz;i++)
	{
	  //      cerr << "i: " <<i<< "p[i] before scale " << p[i] << endl;
	  //       cerr << p[i] << " ";
	  p[i] = double (double(p[i]) / double(tot) );
	  //      cerr << "i: " <<i<< "p[i] after scale " << p[i] << endl;
	  
	}
      //  cerr<<endl;

      RandLibObj.SetDiscreteLookup(p,sz);
    }
  else
    {
      p[1]=1;
      RandLibObj.SetDiscreteLookup(p,1);
      lmales.resize(0);
    }
  //  cerr << "number of males returned from gamete vector routine "<<lmales.size()<<endl;
  delete [] p;
  return lmales;
}


/** 


This method returns a vector of males and initializes a vector of probs of choosing each male as a father.
This method make an approximation based on pulling a single distance from a pollen kernel, then 
finding the individuals within a distance band.  This should be faster than the more exact solution because 
there is only a single random variate pulled and squaring is faster than pow()

*/
vector< PackedIndividual_space >  Landscape_space::CalculateMaleGameteClassVectorApproxDist(PackedIndividual_space pi)
{
  int i,sz;
  //  int dclass = nhab * s;
  int found = 0;

  double mx = pi.GetX(); //coordinates of momma
  double my = pi.GetY();
  double asqr, bsqr;

  vector< PackedIndividual_space > lmales;
  lmales.reserve(1);

  double dist, diff, mindiff;

  sz = valid_males.size();

  //randomly choose a distance from the correct distribution
  dist = pow(RandpollenKernelDensity(),2); //this is the squared distance (Saves calcs in loop)
  
  found=0;
  mindiff = 1000000000;
  for (i=0;i<sz;i++)
    {
      asqr = pow(mx-valid_males[i].GetX(),2);
      bsqr = pow(my-valid_males[i].GetY(),2);
	      
      diff = fabs((asqr + bsqr) - dist);
      
      if (mindiff>diff)
	{
	  if ((asqr==0)&(bsqr==0))
	    {
	      if (self>RandLibObj.uniform())
		{
		  mindiff = diff;
		  found = i;
		}
	    } else 
	    {
	      mindiff = diff;
	      found = i;
	    }
	}

      //      cerr << "dist "<<dist<<" asqr "<<asqr<<" bsqr "<<bsqr<<" diff "<<diff<<" mindiff " <<mindiff<<endl;
      //      cerr << "found "<<found<<endl;
    }
  //  cerr << "found = "<<found<<endl;
  lmales.push_back(valid_males[found]);
  double p[1];  //next two lines quick hack to use setdiscretelookup
  p[0] = 1.0;
  RandLibObj.SetDiscreteLookup(p,1);
  //  cerr << "number of males returned from gamete vector routine "<<lmales.size()<<endl;
  return lmales;
}





  /**
Takes an expression matrix whose columns are phenotypic traits and whose rows are loci present in 
the genotype.  Each element is the proportion of that trait determined by each locus, so columns should sum to 1.

The actual effect of each locus is the sum of alleles at the locus times the appropriate element in the exp matrix.  

There should be the same number of rows as loci regardless if the loci are used
There should be the same number of columns as phenotypes requested

hsq is the 'heritability'.  It is the inverse of the sd of a noise function that is added to the phenotypes
this should be a vector that is 

   **/

std::vector< double > Landscape_space::IndividualPhenotype(PackedIndividual_space ind)
{
  Allele ali;
  size_t np ;
  np=expmat[1].size();
  size_t nl ;
  nl=expmat.size();
  std::vector< double > rvec;
  double ac;
  int p,l, st;

  rvec.resize(np);

  if (hsq.size()!=np) 
    {
#ifdef DEBUG
      cerr<<"mismatched heritabilities and phenotype dimensions"<<endl;
#endif
      assert(0==1);}
  for (p=0;p<int(np);p++)
    {
      ac=0.0;
      for (l=0;l<int(nl);l++)
	{
	  if (Atbls[l]->getClassType()==STEPALLELETBL)
	    {
	      LocusGetAlleleRef(l,ind.GetAllele(l,0),&ali);
	      st=ali.GetState();
	      if (LocusGetPloidy(l)>1)
		{
		  LocusGetAlleleRef(l,ind.GetAllele(l,1),&ali);
		  st=st + ali.GetState();
		}
	      ac = ac+(st * expmat[l][p]);
	    }
	}
      rvec[p] = ac + RandLibObj.normal(0, (ac * (1-hsq[p])));
    }
  return rvec;
}

PackedIndividual_space Landscape_space::FindMate(PackedIndividual_space pi)
{
  PackedIndividual_space tmpI;

  int mc;

  mc = RandLibObj.PickMultinomial();
  tmpI.SetClass(-1);

  tmpI = I[mc].GetRandomInd();

  return tmpI;
}

void Landscape_space::testfindmate(PackedIndividual_space pi)
{
  PackedIndividual_space mate;
  size_t i;
#ifdef DEBUG
  cerr << "target ind: " <<pi<<endl;
#endif
  for (i=1;i<100;i++)
    {
      mate = FindMate(pi);
#ifdef DEBUG
      cerr <<"potential mate # "<<i<<" :  "<< mate<<endl;
#endif
    }
}


///takes an x and y coordinate and returns the population that contains that coordinate
///if found outside of any population, returns -1
int  Landscape_space::getpopulation(double x, double y)
{
  int i;
  int pop;
  pop=-1;
  i=0;
  while ((i<nhab)&&(pop<0))
    {
      if (((x>=popleftx[e][i])&&(x<=poprightx[e][i]))&&((y>=popboty[e][i])&&(y<=poptopy[e][i])))
	{
	  pop=i;
	}
      i++;
    }
  return pop;
}


/**

    This would be the method to override if you were to add a feedback
    between genotype and offspring production/dispersal
    characteristics.  By override, I mean make a class that inherits
    everything from class Landscape_space_statistics (Landscape_space would work
    too).  All you need to do is modify Reproduce(), everything else
    would be inherited from the parent class.  Please don't modify
    Landscape_space or LAndscape_statistics, because there are software that
    depend upon it.

    There are some verbose notes below on how to convert for selection.

 */

void Landscape_space::Reproduce()
{
  PackedIndividual_space tmpI, mate, searchI;
  vector < double > pvec;
  vector < PackedIndividual_space > males ;
  vector< PackedIndividual_space > tmpmales;

  //  vector < PackedIndividual_space >::iterator ti ;
  double tmpx, tmpy;
  int err, indx;
  int q,noff ;
  int bsecls ;
  size_t j, i, k, l, sz, lsz ;
  sz = nhab * s;

  
  for (k=0;k<sz;k++)
    {
            
      //cerr <<"checkpoint in reproduce"<<endl;
      if (R[e].AnyFrom(k)) ///find out if offspring can be produced by this class
	{
	  R[e].SetFromState(k);
	  tmpmales.clear();
	  valid_males.clear();
	  M[e].SetToState(k);
  ///get the individuals in all classes that can contribute male gametes 
  ///and put them in a single vector
	  if (self<1)  //if selfing is not complete, then make a vector of potential fathers
	    {
	      for (i=0;i<sz;i++)
		{ 
		  M[e].SetFromState(i);
		  if (M[e].Value()>0)
		    {
		      tmpmales = I[i].ReturnAsVector();
		      valid_males.insert(valid_males.end(),tmpmales.begin(),tmpmales.end()); //Trying to add all the males to a single vector here....
		    }
		}
	  //randomize the males so that we can just truck through them and pick the first one
	  //that falls in the correct area
	  //randomly choose a distance from the correct distribution
	      std::random_shuffle(valid_males.begin(),valid_males.end()); 
	    } //end if self<1

	  I[k].CompressClass(0.5);///save space (may help speed )
	  I[k].ResetIndividuals();///set an internal pointer to I[k] first ind in list
	  
	  lsz=I[k].size();
	  for (l=0;l<lsz;l++)
	    {
	      searchI = I[k].GetCurrentIndividual();
	      if (searchI.GetClass()<0)
		{
#ifdef DEBUG
		  cerr << "no individual returned from deomgraphic class"<<endl;
#endif
		  assert(searchI.GetClass()==0);
		}
	      indx = I[k].GetCurrentIndex();
	      ///decides where pollen comes from 
	      males.clear();

	      if (self<1) //if selfing is guaranteed, then don't waste time finding fathers
		//		males = CalculateMaleGameteClassVectorApproxDist(searchI); //this is the approximate solution (dist method)
		males = CalculateMaleGameteClassVector(searchI); //this is the more exact solution
	      else
		males.push_back(searchI);
/**

Iterate through the possible new classes of offspring.  By this I mean
the classes to which offspring could disperse.  For each possible
class, choose a number of offspring to put in that class.

After the number of offspring are chosen, they are generated by
finding a male from a multinomial distribution given by the
appropriate column of the M[e] matrix.

***** This approach is definitely wrong if you want to link in
***** selection.  To do that I think you would have to:

1) take the external multinomial vector of potential dispersal sites
given in the R[e] matrix columns.  In the current implementation,
these represent means of poisson distributions.  They could also be
modified to be probabilities of dispersal to a particular distance
(realized dispersal).  Either way, these vectors must be modified or
generated anew for each offspring produced.

2) The modification has to depend upon the genotype of the offspring, 
of course   the simplest way would be to look at the offsprings genotype 
and generate a multinomial probability dist of dispersing to a site.  That 
multinomial distribution would, no doubt be determined by a continuous 
pdf based upon dispersal mechanics.  A nice improvement would be the ability 
to modify an underlying vector of environmental distances specified in the R[e] 
matrix input by the user using a distribution produced by the genotype.  

3) As for the expression of genotype, I would start with an additive model.  
There are two classes of numerically based loci already.  InfAlleles increase 
their integer state monotonically from 0..N.  Not really very useful.  
StepAlleles implemented in the StepAlleleTable are better.  These alleles' 
states drift back and forth from the current state.  This class
implements a strict stepwise mutation model. You could also define
your own mutation model.  Anyway, I would take the sum of the
numerical value of the allele states across all loci involved.  This
single number would determine phenotype.  You could add noise to
simulate developmental/environmantal differences among genotypes

The types of functions that you would need to use on individuals
(PackedIndividuals) are described in DemoClass.h.  Remeber, you can
ResetIndividuals() and reset the pointer to the first ind.  Then you
can iterate through the list by getting the NextIndivdual.

Once you have an individual in hand, use the methods in PackedIndividual.h 
to change it, look at it or its alleles, etc.  Try not to change 
PAckedIndividual.h


*/
	      for (j=0;j<sz;j++)
		{
		  R[e].SetToState(j);
		  ///pick a number of offspring from a 
		  ///Poisson dist with mean=R[tostate,fromstate]

		  if (males.size()==0)
		    {
		      noff=0;
		    }
		  else
		    {
		      noff = R[e].PoissonOffspring();
		    }
		  if (noff>0)
		    {
		      I[k].SetCurrentLastRep(t);
		      I[k].SetCurrentNumOff(noff);

/*
choosing mate.  At this point the effects of genotype upon the mates
ability to produce pollen could be inserted.
*/
		      if (!multiple_paternity)///all offspring from one father
			{
			  if (RandLibObj.uniform()<self)
			    {
			      mate = searchI;
			    }
			  else
			    {
			      ///the lookup table for PickMultinomial is set in the function
			      ///CalculateMaleGameteVector
			      mate = males[RandLibObj.PickMultinomial()];
			    }
			}
		      //cerr<<"abou to generate offspring j " << j <<endl;;
		      for (q=0;q<noff;q++)
			{

			  ///decide on the xy coords of the new offspring
			  //RandLibObj.negexp_xy(searchI.GetX(),searchI.GetY(),seed_mu,tmpx,tmpy);
			  ///if strictly neutral

			  if (nphen==0)
			    {
			      new_propagule_xy(searchI.GetX(),searchI.GetY(),searchI.GetClass(),asp,tmpx,tmpy);
			    }
			  else///selection on phenotype of the mother "searchI"
			    {
			      if (nphen==1)
				{
				  double mult1 = IndividualPhenotype(searchI)[0]/(SELECTIONMAXA*2) ;
				  RandLibObj.rmixed_xy(searchI.GetX(),searchI.GetY(),
						      seed_mu,seed_mu2*mult1,seed_shape2,
						      seed_mix,asp,
						      tmpx,tmpy);
				}
			      else if (nphen==2)
				{
				  double mult1 = 1+(0.5 - IndividualPhenotype(searchI)[0]/(SELECTIONMAXA*2)) ;
				  if (mult1<0) {mult1=0;}
				  double mult2  = 1+(0.5-IndividualPhenotype(searchI)[1]/(SELECTIONMAXA*2)) * seed_mix;
				  if (mult2<0) {mult2=0;}
				  double nmix = mult2*seed_mix;
				  if (nmix>1){nmix=1;}
				  RandLibObj.rmixed_xy(searchI.GetX(),searchI.GetY(),
						      seed_mu,seed_mu2*mult1,seed_shape2,nmix,asp,
						      tmpx,tmpy);
				  //cerr<<"determined phenotype mult "<<mult1<<" mult2 "<<mult2<<endl;
				}
			    }
			  ///This is the place where suitable habitat is determined.  
			  ///
			  if (getpopulation(tmpx,tmpy)>=0)
			    {
			      if (multiple_paternity)///each offspring the product of mixed mating
				{
				  if (RandLibObj.uniform()<self)
				    {
				      mate = searchI;
				    }
				  else
				    {
				      int mn = RandLibObj.PickMultinomial();
				      //				      cerr <<"mn "<<mn<<endl;
				      mate = males[mn];
				    }
				}

			      ///"do the deed" between searchI and mate. tmpI is the baby.  This is where the
			      ///Mendelian genetics plus mutation takes place
			      //			      cerr << "mother "<<searchI;
			      //			      cerr <<"mate "<<mate <<endl;
			      tmpI = searchI.repro_sex(searchI,mate,t,Atbls);
			      ///this could/should be made user selectable
			      bsecls = j - (Habitat(j) * s) ; 
			      tmpI.SetClass((bsecls + (getpopulation(tmpx,tmpy) * s)));

			      tmpI.SetSex(0);
			      tmpI.SetGen(t) ;
			      tmpI.Change(-1);
			      //			      cerr<<"about to birth...";

			      tmpI.Birth(t,Atbls) ;

			      //			      cerr<<"birthed"<<endl;
			      //  cerr << "Seed mu "<<seed_mu<<endl;
			      tmpI.SetX(tmpx);
			      tmpI.SetY(tmpy);
			      //			      tmpI.SetSub(getsubpopulation(tmpI.GetX(),tmpI.GetY())) ;

			      ///keep track of mother's location
			      tmpI.SetMX(searchI.GetX());
			      tmpI.SetMY(searchI.GetY());
			      ///keep track of father's location
			      tmpI.SetFX(mate.GetX());
			      tmpI.SetFY(mate.GetY());
			      err = 0;			      
			      if (I[tmpI.GetClass()].AddIndividual(tmpI)<0)
				{
#ifdef DEBUG
				  cerr << "adding an individual failed" << endl;
#endif
				}
			      
			    }
			} //q
		    }//end if noff>0
		} //j
	      RandLibObj.FreeDiscreteLookup();
	      I[k].NextIndividual();
	    }//l
	} //if R[e].AnyFrom
    }//k 

}//end of function Reproduce




void Landscape_space::Extirpate()
{
  int h,etrue;
  size_t cl,sz;
  double rn;

  std::vector<int> p;

  p.resize(nhab);

  sz = nhab * s;

  etrue=0;
  for (h=0;h<nhab;h++)
    {
      rn = RandLibObj.uniform();
      if (evec[e][h]>rn)
	{
	  p[h]=1;
	  etrue=1;
	}
    }
  if (etrue)
    {
      for (cl=0;cl<sz;cl++)
	{
	  if (p[Habitat(cl)])
	    {
	      I[cl].ClearClass(t,Atbls);
	    }
	}
    }
}

void Landscape_space::CarryState(size_t maxsz, int i)
{
  int numdel,k;

  if (maxsz<I[i].size())
    {
      numdel = (I[i].size()-maxsz);
      for (k=0;k<numdel;k++)
	{
	  I[i].RemoveRandomInd(t,Atbls);
	}
    }
}

void Landscape_space::HabCarry(int k)
{
  int h;
  size_t j, sz;

  std::vector <double> prop;

  sz = nhab * s;
  prop.resize(nhab);

  for (h=0;h<nhab;h++)
    {
      if (k<0)
	{
	  prop[h] = double(kvec[e][h])/double(PopSize(h));
	}
      else
	{
	  prop[h] = double(k)/double(PopSize(h));
	}
      if (prop[h]>1) {prop[h]=1.0;}
    }
  
  for (j=0;j<sz;j++)
    {
      h = Habitat(j);
      CarryState(size_t((prop[h])*I[j].size()),j);
    }
}


void Landscape_space::HabCarry_stg0(int k)
{
  int h;
  size_t j, sz, c;

  sz = nhab * s;

  for (j=0;j<sz;j++)
    {
      if ((j % s)==0)
	{
	  h = Habitat(j);
	  c = kvec[e][h] - (PopSize(h)-I[j].size());
	  if (c<0) {c=0;}
	  CarryState(c,j);
	}
    }
}

/*

void Landscape_space::CarryStateNDD(size_t maxsz, int i)
{
  int numdel,k;
  int sz I[i].size();


  if (maxsz<sz)
    {
      numdel = (sz-maxsz);
      for (k=0;k<sz;k++)
	{
	  
	}     
      for (k=0;k<numdel;k++)
	{
	  I[i].RemoveRandomInd(t,Atbls);
	}
    }
}


void Landscape_space::HabCarry_stg0_NDD(int k)
{
  int h;
  size_t j, sz, c;

  sz = nhab * s;

  for (j=0;j<sz;j++)
    {
      if ((j % s)==0)
	{
	  h = Habitat(j);
	  c = kvec[e][h] - (PopSize(h)-I[j].size());
	  if (c<0) {c=0;}
	  CarryStateNDD(c,j);
	}
    }
}
*/

void Landscape_space::LandCarry()
{
  size_t j, sz;
  double pr;

  sz = nhab * s;
  pr =  double(maxlandsz)/double(PopSize(-1));
  for (j=0;j<sz;j++)
    {
      CarryState(size_t(pr*I[j].size()),j);
    }
}

ostream &Landscape_space::WriteLoci(ostream &stream)
  {
    stream << Atbls <<endl;
    return stream;
  }

ostream &operator<<(ostream &stream, Landscape_space &l)

{
  int ie, id, it, ip, i;
  size_t j, sz;
  
  sz = l.nhab * l.s;
  
  stream << "nhab      " <<" "<< l.nhab << endl;
  stream << "stages    " <<" "<< l.s << endl;
  stream << "ndemo     " << " " << l.ndemo <<endl;
  stream << "rdemo     " << " " << l.rdemo <<endl;
  stream << "nepochs   " <<" "<< l.nep<< endl;
  stream << "cepoch    " <<" "<< l.e<< endl;
  stream << "repoch    " <<" "<< l.randepoch<< endl;
  stream << "ngen      " <<" "<< l.ngen<< endl;
  stream << "cgen      " <<" "<< l.t<< endl;
  stream << "seedmu    " <<" "<< l.seed_mu<< endl;
  stream << "seedmu2   " <<" "<< l.seed_mu2<< endl;
  stream << "seednmu    " <<" "<< l.seed_nmu<< endl;
  stream << "seednmu2   " <<" "<< l.seed_nmu2<< endl;
  stream << "seedshp   " <<" "<< l.seed_shape<< endl;
  stream << "seedshp2  " <<" "<< l.seed_shape2<< endl;
  stream << "seednshp   " <<" "<< l.seed_nshape<< endl;
  stream << "seednshp2  " <<" "<< l.seed_nshape2<< endl;
  stream << "seedmix   " <<" "<< l.seed_mix<< endl;
  stream << "seednmix   " <<" "<< l.seed_nmix<< endl;
  stream << "polmu     " <<" "<< l.pollen_mu<< endl;
  stream << "polshp    " <<" "<< l.pollen_shape<< endl;
  stream << "self      " <<" "<< l.self<< endl;
  stream << "multp     " <<" "<< l.multiple_paternity<< endl;
  stream << "maxlandsz " <<" "<< l.maxlandsz<< endl;

  stream << endl;

  stream << "epochvec  " <<" "<< endl;
  for (ie=0;ie<l.nep;ie++)
    {
      stream << l.epochs[ie] << " " ;
    }

  stream << endl;

  stream << "epochprobs" <<" "<< endl;
  for (ie=0;ie<l.nep;ie++)
    {
      stream << l.epochprobs[ie] << " " ;
    }
  stream << endl;

  stream<< "matrices  " << " " << endl;
  for (id=0;id<l.ndemo;id++)
    {
      stream << l.LM[id] << endl;
    }
  for (ie=0;ie<l.nep;ie++)
    {
      stream << l.S[ie] << l.R[ie] << l.M[ie] << endl ;

      for (id=0;id<l.ndemo;id++)
	{
	  stream << l.demoProbVec[ie][id] << " " ;
	}
      stream << endl;
      for (it=0;it<l.nhab ;it++)
	{
	  stream << l.evec[ie][it] << " " ;
	}
      stream << endl ;
      for (it=0;it<l.nhab ;it++)
	{
	  stream << l.kvec[ie][it] << " " ;
	}

      stream << endl ;
      for (it=0;it<l.nhab ;it++)
	{
	  stream << l.popleftx[ie][it] << " " ;
	}

      stream << endl ;
      for (it=0;it<l.nhab ;it++)
	{
	  stream << l.poprightx[ie][it] << " " ;
	}

      stream << endl ;
      for (it=0;it<l.nhab ;it++)
	{
	  stream << l.poptopy[ie][it] << " " ;
	}

      stream << endl ;
      for (it=0;it<l.nhab ;it++)
	{
	  stream << l.popboty[ie][it] << " " ;
	}

      stream << endl << endl;
    }

  stream << "expression  "<<l.nphen<<endl;

  for (it=0;it<l.getloci();it++)
    {
      for (ip=0;ip<l.nphen;ip++)
	{
	  stream<<l.expmat[it][ip] <<" ";
	}
      stream<<endl;
    }
  stream<<endl;

  for (ip=0;ip<l.nphen;ip++)
    {
      stream<<l.hsq[ip] <<" ";
    }


  stream << "loci        "<<l.getloci()<<endl;

  for (i=0;i<l.getloci();i++)
    {
      l.Atbls[i]->CalcProps();
      l.Atbls[i]->GCAlleles();

      if (l.Atbls[i]->getClassType()==INFALLELETBL)
	{
	  stream << 0 << endl;
	}
      else if (l.Atbls[i]->getClassType()==STEPALLELETBL)
	{
	  stream << 1 << endl;
	}
      else if (l.Atbls[i]->getClassType()==SEQALLELETBL)
	{
	  stream << 2 << endl;
	}
      else
	{
#ifdef DEBUG
	  cerr << "Don't understand the ID of this locus " <<endl;
#endif
	  assert(0==1);
	}
      l.Atbls[i]->Write(stream);
      stream << endl;
    }
  
  stream <<     "individ   " << " " << l.PopSize(-1) << endl;
  for (j=0;j<sz;j++)
    {
	  stream << l.I[j];
    }
  return stream;
}


istream &operator>>(istream & stream, Landscape_space &l)
{
  //  size_t sz;
  int i, j, ip, ni, h, tmpi, nl ;
  int indflag=0;
  int ktot,maxk;
  std::vector <int> p;
  double totprob;


  PackedIndividual_space ind;
  
  Allele tmpallele;
  
  string c,tmps,alist,nlist,wlist ;
  int epochprobs = 0;
  char tc;
  char cp[256];

  alist = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
  nlist = "e-+0123456789.";
  wlist = " \n\t\f";

  nl=1;

  l.init();

  while (!stream.eof())
    {
      tc = stream.peek();
      if (short(tc)>=0)
	{
	  if (wlist.find(tc)==string::npos && nl) //no white space
	    {
	      nl=0;
	      if (!(tc=='#')) {
		      
		c.erase(c.begin(),c.end());
		for (i=0;i<TOKENLEN;i++)
		  {
		    stream.get(tc);
		    if (tc!=' ')
		      {
			tmps=tc;
			c.append(tmps);
		      }
		    if (stream.eof()) {break;}
		  }
		      
		if      (c=="nhab")
		  {
		    stream >> l.nhab;
		  }
		else  if (c=="stages")
		  {
		    stream >> l.s;
		  }
		else if      (c=="repoch")
		  {
		    stream >> l.randepoch;
		    epochprobs = 1;
		  }
		else  if (c=="nepochs")
		  {
		    stream >> l.nep;
		    l.setepochs(l.nep);
			  
		  }
		else  if (c=="cepoch")
		  {
		    stream >> l.e;
		  }
		else if      (c=="ndemo")
		  {
		    stream >> l.ndemo;
		    if (l.ndemo==0)
		      {
#ifdef RDEBUG
			cerr << "chose not to use the habitat-level demography specification"<<endl;
#endif
		      }
#ifdef RDEBUG
  cerr << "Running l.setndemo in the landcape inserter "<<l.s <<endl;
#endif
		    l.setndemo(l.ndemo);
		  }
		else if      (c=="rdemo")
		  {
		    stream >> l.rdemo;
		    if (l.rdemo)
		      {
#ifdef RDEBUG
			cerr << "chose to randomly assign habitat-level demography specification"<<endl;
#endif
		      }
		  }
		else if (c=="ngen")
		  {
		    stream >> l.ngen;
		  }
		else if (c=="cgen")
		  {
		    stream >> l.t;
		  }
		else if (c=="seedmix")
		  {
		    stream >> l.seed_mix;
		  }
		else if (c=="seednmix")
		  {
		    stream >> l.seed_nmix;
		  }
		else if (c=="seedmu")
		  {
		    stream >> l.seed_mu;
		  }
		else if (c=="seedmu2")
		  {
		    stream >> l.seed_mu2;
		  }
		else if (c=="seednmu")
		  {
		    stream >> l.seed_nmu;
		  }
		else if (c=="seednmu2")
		  {
		    stream >> l.seed_nmu2;
		  }
		else if (c=="seedshp")
		  {
		    stream >> l.seed_shape;
		  }
		else if (c=="seedshp2")
		  {
		    stream >> l.seed_shape2;
		  }
		else if (c=="seednshp")
		  {
		    stream >> l.seed_nshape;
		  }
		else if (c=="seednshp2")
		  {
		    stream >> l.seed_nshape2;
		  }
		else if (c=="polmu")
		  {
		    stream >> l.pollen_mu;
		  }
		else if (c=="polshp")
		  {
		    stream >> l.pollen_shape;
		  }
		else if (c=="self")
		  {
		    stream >> l.self;
		  }
		else if (c=="multp")
		  {
		    stream >> l.multiple_paternity;
		  }
		else if (c=="maxlandsz")
		  {
		    stream >> l.maxlandsz;
		  }
		else if (c=="epochvec")
		  {
		    for (i=0;i<l.nep;i++)
		      {
			stream >> l.epochs[i] ;
		      }
		  }
		else if (c=="epochprobs")
		  {
		    totprob=0;
		    for (i=0;i<l.nep;i++)
		      {
			stream >> l.epochprobs[i] ;
			totprob += l.epochprobs[i];
		      }
		    epochprobs=0;
		    if (totprob!=1)
		      {
#ifdef DEBUG
			cerr << "epoch probabilities do not sum to one" <<endl;;
#endif
			assert(totprob==1);
		      }
		  }
		else if (c=="matrices")
		  {
		    maxk = 0;
			  
		    for (i=0;i<l.ndemo;i++)
		      {
			stream >> l.LM[i];
		      }
			  
		    for (i=0;i<l.nep;i++)
		      {
			ktot = 0;

			stream >> l.S[i] ;
			stream >> l.R[i] ;
			stream >> l.M[i] ;

			for (j=0;j<l.ndemo;j++)
			  {
			    stream >> l.demoProbVec[i][j];
			  }
			for (j=0;j<l.nhab;j++)
			  {
			    stream >> l.evec[i][j] ;
			  }
			for (j=0;j<l.nhab;j++)
			  {
			    stream >> l.kvec[i][j] ;
			    ktot = l.kvec[i][j] + ktot;
			  }


			for (j=0;j<l.nhab;j++)
			  {
			    stream >> l.popleftx[i][j] ;
			  }
			for (j=0;j<l.nhab;j++)
			  {
			    stream >> l.poprightx[i][j] ;
			  }
			for (j=0;j<l.nhab;j++)
			  {
			    stream >> l.poptopy[i][j] ;
			  }
			for (j=0;j<l.nhab;j++)
			  {
			    stream >> l.popboty[i][j] ;
			  }

			if (ktot>maxk) 
			  {
			    maxk=ktot;
			  }
		      }
		  }
		else if (c=="expression")
		  {
		    stream >> l.nphen;
		    for (j=0;j<l.getloci();j++)
		      {
			for (ip=0;ip < l.nphen ;ip++)
			  {
			    stream >> l.expmat[j][ip] ;
			  }
		      }
		    for (ip=0;ip<l.nphen;ip++)
		      {
			stream >> l.hsq[ip] ;
		      }
		  }		      
		else if (c=="loci")
		  {
		    stream >> l.Atbls;
		    l.setloci();
		  }
		      
		else if (c=="individ")
		  {
		    if (!indflag)
		      {
			stream >> ni ;
			assert(ni > 0);
			l.I.resize( l.nhab * l.s );
			for (i=0;i<ni;i++)
			  {
			    l.SetUpInd(ind);
			    stream >> ind;
			    ind.Birth(-1,l.Atbls);
			    l.I[ind.GetClass()].AddIndividual(ind);
			  }
			indflag=1;
		      }
		    else
		      {
#ifdef DEBUG
			cerr << "already defined popsize vectors, can't define inds";
#endif
			assert(1==0);
		      }
		  }
		else if (c=="popinit")
		  {
		    if (!indflag)
		      {
			stream >> h ;
			for (i=0;i<h;i++)
			  {
			    stream >> tmpi;
			    p.push_back(tmpi);
			  }
			l.popsizeset(p);
			indflag=1;
		      }
		    else
		      {
#ifdef DEBUG
			cerr << "already defined individual vectors";
#endif
			assert(0);
		      }
		  }
		else
		  {
#ifdef DEBUG
		    cerr << "unrecognized token `" << c << "' in Landscape inserter"<<endl ;
#endif 
		  }
	      }
	      else
		{
		  stream.getline(cp,256,'\n');
		  nl = 1;
		}
	    }
	  else 
	    {
	      stream.get(tc);
	      if (tc=='\n')
		{
		  nl=1;
		}
	    }
	}
      else
	{
	  break;
	}
    }
  if (epochprobs)
    {
#ifdef DEBUG
      cerr << "asked for random epochs, but did not include probabilities" <<endl; ;
#endif
      assert(1==2);
    }

  return stream;
}



void Landscape_space::Advance()
{
  t++; //Increment generation
  ChooseEpoch();
  ConstructDemoMatrix();
}

void Landscape_space::new_propagule_xy(double ix, double iy, int cls, double aspect, double &x, double &y)
{
  //    cerr << "seedkernel params, class: "<<cls<<" kernel row: "<<SK[e][cls][0]<<", "<<SK[e][cls][1]<<", "<<SK[e][cls][2]<<", "<<SK[e][cls][3]<<", "<<SK[e][cls][4]<<", "<<SK[e][cls][5]<<endl;

  switch (int(SK[e][cls][0]))
    {
    case 1: //exponential (weibull with shape set at 1)
      RandLibObj.rweibull_xy(ix,iy,SK[e][cls][1],1,aspect,x,y);
      //      cerr <<"exp"<<endl;
      break;
    case 2: //weibull
      RandLibObj.rweibull_xy(ix,iy,SK[e][cls][1],SK[e][cls][2],aspect,x,y);
      //      cerr <<"weib"<<endl;
      break;
      ///    case 3: //Clarks formulation of geometric
      ///      RandLibObj.rgeom_xy(ix,iy,SK[e][cls][1],SK[e][cls][2],aspect,x,y);
      ///      cerr <<"geom"<<endl;
      ///      break;
    case 3: //mixture distribution
      //      cerr <<"mixed"<<endl;
      //      cerr << "seedkernel params, class: "<<cls<<" kernel row: "<<SK[e][cls][0]<<", "<<SK[e][cls][1]<<", "<<SK[e][cls][2]<<", "<<SK[e][cls][3]<<", "<<SK[e][cls][4]<<", "<<SK[e][cls][5]<<endl;
      RandLibObj.rassym_mixed_xy(ix,iy,
				 SK[e][cls][1],SK[e][cls][3],
				 SK[e][cls][1],SK[e][cls][3],
				 SK[e][cls][2],SK[e][cls][4],
				 SK[e][cls][2],SK[e][cls][4],
				 SK[e][cls][5],SK[e][cls][5],aspect,x,y);
    }
}

int Landscape_space::male_gamete_source_habitat(double ix, double iy,  double aspect, int numtries)
{
  ///note that the compared to new_propagule_xy, there is no 'cls' parameter
  ///This function assumes that the first row in the PK matrix works for all rows.  This is equivalent of assuming a
  ///single pollen kernel for all males in the landscape

  int cls=0; ///if ever changing to a variable pollen density, this will need to be variable
  int pop;
  double x,y;
  do
    {
      switch (int(PK[e][cls][0]))
	{
	case 1: //exponential (weibull with shape set at 1)
	  RandLibObj.rweibull_xy(ix,iy,PK[e][cls][1],1,aspect,x,y);
	  //      cerr <<"exp"<<endl;
	  break;
	case 2: //weibull
	  RandLibObj.rweibull_xy(ix,iy,PK[e][cls][1],PK[e][cls][2],aspect,x,y);
	  //      cerr <<"weib"<<endl;
	  break;
	  ///    case 3: //Clarks formulation of geometric
	  ///      RandLibObj.rgeom_xy(ix,iy,PK[e][cls][1],PK[e][cls][2],aspect,x,y);
	  ///      cerr <<"geom"<<endl;
	  ///      break;
	case 3: //mixture distribution
	  //      cerr <<"mixed"<<endl;
	  //      cerr << "seedkernel params, class: "<<cls<<" kernel row: "<<PK[e][cls][0]<<", "<<PK[e][cls][1]<<", "<<PK[e][cls][2]<<", "<<PK[e][cls][3]<<", "<<PK[e][cls][4]<<", "<<PK[e][cls][5]<<endl;
	  RandLibObj.rassym_mixed_xy(ix,iy,
				     PK[e][cls][1],PK[e][cls][3],
				     PK[e][cls][1],PK[e][cls][3],
				     PK[e][cls][2],PK[e][cls][4],
				     PK[e][cls][2],PK[e][cls][4],
				     PK[e][cls][5],PK[e][cls][5],aspect,x,y);
	}    
      ///test if there is habitat at x and y.  If there is, check if there are males in that habitat
      ///if there is not, then pull another male gamete (pollen grain)
      pop=getpopulation(x,y);
    }
  while (pop<0);
  return(pop);
}

double Landscape_space::pollenKernelDensity(double dist, int i)
{
  double res=0;
  //   cerr << "pollenkernel params, class: "<<i<<" dist "<<dist<<" kernel row: "<<PK[e][i][0]<<", "<<PK[e][i][1]<<", "<<PK[e][i][2]<<", "<<PK[e][i][3]<<", "<<PK[e][i][4]<<", "<<PK[e][i][5]<<endl;
  switch (int(PK[e][i][0]))
    {
    case 1: //exponential (weibull with shape set at 1)
      res = RandLibObj.weibull(dist,PK[e][i][1],1);
      break;
    case 2: //weibull
      res =RandLibObj.weibull(dist,PK[e][i][1],PK[e][i][2]);
      break;
      ///   case 3: //Clarks formulation of geometric
      ///      res = RandLibObj.geom(dist,PK[e][i][1],PK[e][i][2]);
      ///      break;
    case 3: //mixture distribution
      res = RandLibObj.mixed(dist,PK[e][i][1],PK[e][i][3],PK[e][i][2],PK[e][i][4],PK[e][i][5]);
    }  
  //    cerr <<"res = "<<res<<endl;
  return res;
}



double Landscape_space::RandpollenKernelDensity()
{
  double res=0;
  int i=0;
  //   cerr << "pollenkernel params, class: "<<i<<" dist "<<dist<<" kernel row: "<<PK[e][i][0]<<", "<<PK[e][i][1]<<", "<<PK[e][i][2]<<", "<<PK[e][i][3]<<", "<<PK[e][i][4]<<", "<<PK[e][i][5]<<endl;
  switch (int(PK[e][i][0]))
    {
    case 1: //exponential (weibull with shape set at 1)
      res = RandLibObj.rndweibull(PK[e][i][1],1);
      break;
    case 2: //weibull
      res =RandLibObj.rndweibull(PK[e][i][1],PK[e][i][2]);
      break;
      ///   case 3: //Clarks formulation of geometric
      ///      res = RandLibObj.geom(dist,PK[e][i][1],PK[e][i][2]);
      ///      break;
    case 3: //mixture distribution
      res = RandLibObj.rndmixed(PK[e][i][1],PK[e][i][3],PK[e][i][2],PK[e][i][4],PK[e][i][5]);
    }  
  //    cerr <<"res = "<<res<<endl;
  return res;
}


///begin implementation of Landscape_statistics


Landscape_space_statistics::Landscape_space_statistics (int h, int stg, int loc, int ep, int gn)
{
  ///  init(h,stg, loc, ep, gn);
}

Landscape_space_statistics::~Landscape_space_statistics ()
{
	  
#ifdef RDEBUG
  cerr << "Landscape_space_statistics destructor starting" <<endl;
#endif

#ifdef RDEBUG
  cerr << "Landscape_space_statistics destructor exiting" <<endl;
#endif

}

void Landscape_space_statistics::Statistics(ostream & streamout)
{

  int i,sgz  ;
  sgz = s * nhab;

  streamout << t <<"  " <<e <<" " ;
  for (i=0;i<sgz;i++)
    {
      //      streamout.form("%6i ",I[i].size());
      streamout << setw(6) << I[i].size();
    }
  streamout <<"  "<< PopSize(-1) <<"  "<<endl;;
}


double Landscape_space_statistics::GenLength()
{
  size_t i,sz;
  double gensiz=0.0,tmp;
  double totparents=0.0;
  sz=s*nhab;
  for (i=0;i<sz;i++)
    {
      if (R[e].AnyFrom(i))
	{
	  gensiz = gensiz + I[i].GenLength(t)*I[i].size();
	  totparents= totparents + I[i].size();
	}
    }
  tmp = gensiz/totparents;
  return tmp;
}

void Landscape_space_statistics::ArlequinDiploidOut(int numind, ostream &streamout)
{

  int i,j, k, ss, ps;
  size_t sz;
  vector <PackedIndividual_space> IVec;
  vector <int> diptbl;

  //find the number of occupied habitats

  IVec.reserve(numind);
  diptbl.reserve(nloc);
  
  for (k=0;k<nloc;k++)
    {
      if (Atbls[k]->getPloidy()==2)
	{
	  diptbl.push_back(k);
	}
    }

  j=0;

  streamout << "# output from Metasim "<<endl;
  streamout << "# number of stages per habitat: "<< s << endl;
  streamout << "# number of habitats: "<< nhab << endl;
  streamout << "# current sizes of the habitats (in order): "<<endl<<"# ";
  for (i=0;i<nhab;i++)
    {
      streamout << PopSize(i) << "   ";
      if (PopSize(i)>0)
	{
	  j++;
	}
    }
  streamout << endl;

  streamout << "# Number of generations elapsed (current gen): "<<t<<endl;
  streamout << "# Begin dataset "<<endl;

  streamout << "[Profile] "<< endl;
  streamout << "Title = \""<<title<<".  Generation "<<t << "\"" <<endl;
  streamout << "NbSamples = "<< j <<endl;
  streamout << "DataType = STANDARD " <<endl;
  streamout << "GenotypicData=1 " <<endl;
  streamout << "LocusSeparator = WHITESPACE" <<endl;
  streamout << "GameticPhase=0 " <<endl;
  streamout << "RecessiveData=0 " <<endl;

  streamout << "[Data]" <<endl;
  streamout << "[[Samples]]" <<endl;
  for (i=0;i<nhab;i++)
    {
      ps = PopSize(i);
      if (ps>0)
	{
	  IVec.resize(0);
	  for (j=i*s;j<((i*s)+s);j++)
	    {
	      I[j].ResetIndividuals();
	      for (sz=0;sz<I[j].size();sz++)
		{
		  IVec.push_back(I[j].GetCurrentIndividual());
		  I[j].NextIndividual();
		}
	    }
	  random_shuffle(IVec.begin(),IVec.end());
	  streamout << "Samplename = \"Pop_"<< i << "\"" << endl;
	  if (ps>numind)
	    {
	      streamout << "SampleSize = "<<numind<<endl;
	      ss = numind;
	    }
	  else
	    {
	      streamout << "SampleSize = "<< ps <<endl;
	      ss = ps;
	    }
	  streamout << "SampleData = {"<<endl;
	  
	  for (j=0;j<ss;j++)
	    {
	      streamout << "Ind_"<<j<<"_Cl_"<<IVec[j].GetClass()<<" 1 \t";
	      for (sz=0;sz<diptbl.size();sz++)
		{
		  streamout << IVec[j].GetAllele(diptbl[sz],0) << " \t";
		}
	      streamout << endl << "        \t";
	      for (sz=0;sz<diptbl.size();sz++)
		{
		  streamout << IVec[j].GetAllele(diptbl[sz],1) << " \t";
		}
	      streamout << endl;
	    }
	  streamout << "}"<<endl;
	}
    }
}



void Landscape_space_statistics::ArlequinHaploidOut(int numind, ostream &streamout)
{

  int i,j, k, ss, ps;
  size_t sz;
  vector <PackedIndividual_space> IVec;
  vector <int> diptbl;
  map<string, int, ltstr > htypes ;
  map<string, int, ltstr >::iterator tmpiter ;
  ostringstream *strstrmp;
  string strng;

  //find the number of occupied habitats

  IVec.reserve(numind);
  diptbl.reserve(nloc);
  
  for (k=0;k<nloc;k++)
    {
      if (Atbls[k]->getPloidy()==1)
	{
	  diptbl.push_back(k);
	}
    }

  j=0;

  streamout << "# output from Metasim "<<endl;
  streamout << "# number of stages per habitat: "<< s << endl;
  streamout << "# number of habitats: "<< nhab << endl;
  streamout << "# current sizes of the habitats (in order): "<<endl<<"# ";
  for (i=0;i<nhab;i++)
    {
      streamout << PopSize(i) << "   ";
      if (PopSize(i)>0)
	{
	  j++;
	}
    }
  streamout << endl;

  streamout << "# Number of generations elapsed (current gen): "<<t<<endl;
  streamout << "# Begin dataset "<<endl;

  streamout << "[Profile] "<< endl;
  streamout << "Title = \""<<title<<".  Generation "<<t << "\"" <<endl;
  streamout << "NbSamples = "<< j <<endl;
  streamout << "DataType = STANDARD " <<endl;
  streamout << "GenotypicData=0 " <<endl;
  streamout << "LocusSeparator = WHITESPACE" <<endl;
  streamout << "GameticPhase=1 " <<endl;
  streamout << "RecessiveData=0 " <<endl;

  streamout << "[Data]" <<endl;
  streamout << "[[Samples]]" <<endl;
  for (i=0;i<nhab;i++)
    {
      ps = PopSize(i);
      if (ps>0)
	{
	  IVec.resize(0);
	  for (j=i*s;j<((i*s)+s);j++)
	    {
	      I[j].ResetIndividuals();
	      for (sz=0;sz<I[j].size();sz++)
		{
		  IVec.push_back(I[j].GetCurrentIndividual());
		  I[j].NextIndividual();
		}
	    }
	  random_shuffle(IVec.begin(),IVec.end());
	  streamout << "Samplename = \"Pop_"<< i << "\"" << endl;
	  if (ps>numind)
	    {
	      streamout << "SampleSize = "<<numind<<endl;
	      ss = numind;
	    }
	  else
	    {
	      streamout << "SampleSize = "<< ps <<endl;
	      ss = ps;
	    }
	  streamout << "SampleData = {"<<endl;
	  
	  for (j=0;j<ss;j++)
	    {
	      strstrmp = new ostringstream;
	      for (sz=0;sz<diptbl.size();sz++)
		{
		  IVec[j].WriteState(diptbl[sz],0,Atbls,*strstrmp) ;
		  *strstrmp << " ";
		}
	      *strstrmp << ends;
	      strng = strstrmp->str();
	      tmpiter = htypes.find(strng);
	      if (tmpiter!=htypes.end())
		{
		  (*tmpiter).second=(*tmpiter).second++;
		}
	      else
		{
		  htypes[strng]=1;
		}
#ifdef DEBUG
	      cerr << strng <<endl;
#endif
	      delete strstrmp;
	    }

	  j=1;
	  for (tmpiter=htypes.begin();tmpiter!=htypes.end();tmpiter++)
	    {
	      streamout << "Hap_"<<j << "_"<<i<<"  "<<(*tmpiter).second<<" "<<(*tmpiter).first<<endl;
	      j++;
	    }
	  streamout << "}"<<endl;
	  htypes.clear();
	}
    }
}


void Landscape_space_statistics::GenepopOut(int numind, ostream &streamout)
{

  int i,j, k, q, ss, ps;
  size_t sz;
  vector <PackedIndividual_space> IVec;
  vector <int> diptbl;

  //find the number of occupied habitats

  IVec.reserve(numind);
  diptbl.reserve(nloc);
  
  for (k=0;k<nloc;k++)
    {
      diptbl.push_back(k);
    }

  j=0;

  for (i=0;i<nhab;i++)
    {
      if (PopSize(i)>0)
	{
	  j++;
	}
    }
  streamout << "Metasim output";
  streamout << ": stages: "<< s ;
  streamout << ": habitats: "<< nhab << "  occupied: "<<j;
  streamout << ": generations: "<<t<<endl;
  for (sz=0;sz<diptbl.size();sz++)
    {
      streamout << "L"<<diptbl[sz]<<"Pl"<<Atbls[diptbl[sz]]->getPloidy()<<"mu"<<Atbls[diptbl[sz]]->getMutationRate() <<endl;
    }

  for (i=0;i<nhab;i++)
    {
      ps = PopSize(i);
      if (ps>0)
	{
	  IVec.resize(0);
	  for (j=i*s;j<((i*s)+s);j++)
	    {
	      I[j].ResetIndividuals();
	      for (sz=0;sz<I[j].size();sz++)
		{
		  IVec.push_back(I[j].GetCurrentIndividual());
		  I[j].NextIndividual();
		}
	    }
	  random_shuffle(IVec.begin(),IVec.end());

	  streamout << "POP" << endl;

	  if (ps>numind)
	    {
	      ss = numind;
	    }
	  else
	    {
	      ss = ps;
	    }
	  
	  for (j=0;j<ss;j++)
	    {
	      streamout << "Pop"<<i<<" Size: "<<PopSize(i)<<" Ind "<<j<<" Cl "<<IVec[j].GetClass()<<",  ";
	      for (sz=0;sz<diptbl.size();sz++)
		{
		  for (q=0;q<Atbls[diptbl[sz]]->getPloidy();q++)
		    {
		      streamout<<setw(2)<<IVec[j].GetAllele(diptbl[sz],q)+1;
		    }
		  streamout << " ";
		}
	      streamout << endl;
	    }
	}
    }
}

void Landscape_space_statistics::GdaOut(int numind, ostream &streamout)
{

  int i,j, k, q, ss, ps, pp=0;
  size_t sz;
  vector <PackedIndividual_space> IVec;
  vector <int> diptbl, haptbl;
  ostringstream poplist;

  //find the number of occupied habitats

  IVec.reserve(numind);
  diptbl.reserve(nloc);
  haptbl.reserve(nloc);
  
  for (k=0;k<nloc;k++)
    {
      if (Atbls[k]->getPloidy()==2)
	{
	  diptbl.push_back(k);
	}
      if (Atbls[k]->getPloidy()==1)
	{
	  haptbl.push_back(k);
	}
    }

  j=0;

  for (i=0;i<nhab;i++)
    {
      if (PopSize(i)>0)
	{
	  j++;
	}
    }

  streamout << "#Nexus"<<endl;
  streamout << "begin gdadata; ";

  streamout << "[Metasim diploid output";
  streamout << ": stages: "<< s ;
  streamout << ": habitats: "<< nhab << "  occupied: "<<j;
  streamout << ": generations: "<<t<<" ]"<<endl;
  streamout << "dimensions npops="<<j<<" nloci="<<haptbl.size()+diptbl.size()<<";"<<endl;
  streamout << "format missing=? separator=/;"<<endl;
  streamout << "hapset 1-"<< haptbl.size()<<";"<<endl;

  streamout << "matrix "<<endl;
  for (i=0;i<nhab;i++)
    {
      ps = PopSize(i);
      if (ps>0)
	{
	  IVec.resize(0);
	  for (j=i*s;j<((i*s)+s);j++)
	    {
	      I[j].ResetIndividuals();
	      for (sz=0;sz<I[j].size();sz++)
		{
		  IVec.push_back(I[j].GetCurrentIndividual());
		  I[j].NextIndividual();
		}
	    }
	  random_shuffle(IVec.begin(),IVec.end());

	  if ((i>0)&&(pp>0))
	    {
	      streamout << endl << "," << endl;
	      poplist << ", ";
	    }
	  streamout << "POP_"<< i <<"_Size_"<<PopSize(i)<<":" << endl;
	  poplist << "POP_"<< i <<"_Size_"<<PopSize(i);

	  if (ps>numind)
	    {
	      ss = numind;
	    }
	  else
	    {
	      ss = ps;
	    }
	  
	  for (j=0;j<ss;j++)
	    {
	      streamout << "Ind_"<<j<<"_Cl_"<<IVec[j].GetClass()<<"  ";
	      for (sz=0;sz<haptbl.size();sz++)
		{
		  streamout <<IVec[j].GetAllele(haptbl[sz],0)+1;
		  streamout << " ";
		}
	      for (sz=0;sz<diptbl.size();sz++)
		{
		  for (q=0;q<Atbls[diptbl[sz]]->getPloidy();q++)
		    {
		      if (q>0)
			{
			  streamout <<"/"; //add in the allele seperator
			}
		      streamout << IVec[j].GetAllele(diptbl[sz],q)+1;
		    }
		  streamout << " ";
		}
	      streamout << endl;
	    }
	  pp++;
	}
    }
  poplist << ends;
  streamout << ";"<<endl<<"end;" << endl;

  streamout << "begin trees;"<<endl<<"tree popstruct = (" << poplist.str() << ");" <<endl<<"end;"<<endl;
}


void Landscape_space_statistics::MigrateDiploidOut(int numind, ostream &streamout)
{

  int i,j, k, ss, ps;
  size_t sz;
  vector <PackedIndividual_space> IVec;
  vector <int> diptbl;

  //find the number of occupied habitats

  IVec.reserve(numind);
  diptbl.reserve(nloc);
  
  for (k=0;k<nloc;k++)
    {
      if (Atbls[k]->getPloidy()==2)
	{
	  diptbl.push_back(k);
	}
    }

  j=0;

  for (i=0;i<nhab;i++)
    {
      if (PopSize(i)>0)
	{
	  j++;
	}
    }
  streamout << j << " " << diptbl.size() << " " ;
  streamout << "Metasim out";
  streamout << ": stg: "<< s ;
  streamout << ": hab: "<< nhab ;
  streamout << ": gen: "<<t<<endl;
  for (i=0;i<nhab;i++)
    {
      ps = PopSize(i);
      if (ps>0)
	{
	  IVec.resize(0);
	  for (j=i*s;j<((i*s)+s);j++)
	    {
	      I[j].ResetIndividuals();
	      for (sz=0;sz<I[j].size();sz++)
		{
		  IVec.push_back(I[j].GetCurrentIndividual());
		  I[j].NextIndividual();
		}
	    }
	  random_shuffle(IVec.begin(),IVec.end());

	  if (ps>numind)
	    {
	      ss = numind;
	    }
	  else
	    {
	      ss = ps;
	    }

	  streamout<<setw(3)<<ss;
	  streamout << "  Pop"<<i<<" Size"<<ps<<"\n";

	  
	  for (j=0;j<ss;j++)
	    {

	      streamout <<"I"<<setw(3)<<"  "<<IVec[j].GetClass();
	      for (sz=0;sz<diptbl.size();sz++)
		{
		  streamout <<" "<<char(IVec[j].GetAllele(diptbl[sz],0)+65);
		  streamout <<char(IVec[j].GetAllele(diptbl[sz],1)+65);
		}
	      streamout << endl;
	    }
	}
    }
}
void Landscape_space_statistics::BiosysDiploidOut(int numind, ostream &streamout)
{

  int i,j, k, ss, ps;
  size_t sz;
  vector <PackedIndividual_space> IVec;
  vector <int> diptbl;
  map <int, int, less <int> > loclook;

  //find the number of occupied habitats

  IVec.reserve(numind);
  diptbl.reserve(nloc);
  
  for (k=0;k<nloc;k++)
    {
      if (Atbls[k]->getPloidy()==2)
	{
	  diptbl.push_back(k);
	}
    }

  j=0;

  for (i=0;i<nhab;i++)
    {
      if (PopSize(i)>0)
	{
	  j++;
	}
    }
  streamout << "Metasim output";
  streamout << ": stages: "<< s ;
  streamout << ": habitats: "<< nhab << "  occupied: "<<j;
  streamout << ": generations: "<<t<<endl;
  streamout << "NOTU=" << j << ",NLOC=" << diptbl.size() << ", NALL=100 ;" << endl;
  streamout << "("<<diptbl.size()<<"(1X,A3))"<<endl;
  for (sz=0;sz<diptbl.size();sz++)
    {
      streamout << "L"<<setw(2)<<diptbl[sz];
    }

  streamout << endl;
  streamout << "STEP DATA:"<<endl;
  streamout << "DATYP=1,NUMER;"<<endl;
  streamout << "(A4, "<<diptbl.size()<<"(1X,I3,I3))"<<endl;
  for (i=0;i<nhab;i++)
    {
      ps = PopSize(i);
      if (ps>0)
	{
	  IVec.resize(0);
	  for (j=i*s;j<((i*s)+s);j++)
	    {
	      I[j].ResetIndividuals();
	      for (sz=0;sz<I[j].size();sz++)
		{
		  IVec.push_back(I[j].GetCurrentIndividual());
		  I[j].NextIndividual();
		}
	    }
	  random_shuffle(IVec.begin(),IVec.end());

	  streamout << "P"<< setw(3) << i << endl;

	  if (ps>numind)
	    {
	      ss = numind;
	    }
	  else
	    {
	      ss = ps;
	    }
	  
	  for (j=0;j<ss;j++)
	    {
	      for (sz=0;sz<diptbl.size();sz++)
		{

		  if (loclook.find(IVec[j].GetAllele(diptbl[sz],0))==loclook.end())
		    {
		      loclook[IVec[j].GetAllele(diptbl[sz],0)] = loclook.size()+1;
		    }
		  if (loclook.find(IVec[j].GetAllele(diptbl[sz],1))==loclook.end())
		    {
		      loclook[IVec[j].GetAllele(diptbl[sz],1)] = loclook.size()+1;
		    }
		}
	    }

	  for (j=0;j<ss;j++)
	    {

	      streamout << "I" <<setw(3)<<IVec[j].GetClass();
	      for (sz=0;sz<diptbl.size();sz++)
		{
		  streamout << setw(3)<<loclook[IVec[j].GetAllele(diptbl[sz],0)]
		            << setw(3)<<loclook[IVec[j].GetAllele(diptbl[sz],1)];
		}
	      streamout << endl;
	    }
	  streamout << "NEXT" <<endl;
	}
    }
  streamout << "END;" <<endl;

  ///adds procedures to the biosys file
  streamout << "STEP VARIAB:"<<endl<<"FULLOUT"<<endl<<"END;"<<endl;
  streamout << "STEP HDYWBG:"<<endl<<"END;"<<endl;
  streamout << "STEP SIMDIS:"<<endl<<"NEI"<<endl<<"END;"<<endl;
  streamout << "STEP COEFOUT:"<<endl<<"ABOVE=1,BELOW=2;"<<endl<<"END;"<<endl;
  streamout << "STEP FSTAT:"<<endl<<"OUTPUT=1;"<<endl<<"END;"<<endl;
  streamout << "STEP HETXSQ:"<<endl<<"END;"<<endl;
}


void Landscape_space_statistics::MicroRatOut(int numind, ostream &streamout )
{

  int i,j, k, q, ss, ps;
  size_t sz;
  vector <PackedIndividual_space> IVec;
  vector <int> diptbl;

  //find the number of occupied habitats

  IVec.reserve(numind);
  diptbl.reserve(nloc);
  
  for (k=0;k<nloc;k++)
    {
      diptbl.push_back(k);
    }

  j=0;

  for (i=0;i<nhab;i++)
    {
      if (PopSize(i)>0)
	{
	  j++;
	}
    }
  streamout << "Metasim output";
  streamout << ": stages: "<< s ;
  streamout << ": habitats: "<< nhab << "  occupied: "<<j;
  streamout << ": generations: "<<t<<endl;
  for (sz=0;sz<diptbl.size();sz++)
    {
      streamout << "L"<<diptbl[sz]<<"Pl"<<Atbls[diptbl[sz]]->getPloidy()<<"mu"<<Atbls[diptbl[sz]]->getMutationRate() <<endl;
    }

  for (i=0;i<nhab;i++)
    {
      ps = PopSize(i);
      if (ps>0)
	{
	  IVec.resize(0);
	  for (j=i*s;j<((i*s)+s);j++)
	    {
	      I[j].ResetIndividuals();
	      for (sz=0;sz<I[j].size();sz++)
		{
		  IVec.push_back(I[j].GetCurrentIndividual());
		  I[j].NextIndividual();
		}
	    }
	  random_shuffle(IVec.begin(),IVec.end());

	  streamout << "POP" << endl;

	  if (ps>numind)
	    {
	      ss = numind;
	    }
	  else
	    {
	      ss = ps;
	    }
	  
	  for (j=0;j<ss;j++)
	    {
	      streamout << "Pop"<<i<<" Ind "<<j<<" Cl "<<IVec[j].GetClass()<<", , , ";
	      for (sz=0;sz<diptbl.size();sz++)
		{
		  for (q=0;q<Atbls[diptbl[sz]]->getPloidy();q++)
		    {
		      streamout<<setw(2)<<IVec[j].GetAllele(diptbl[sz],q)+1;
		    }
		  streamout << " ";
		}
	      streamout << endl;
	    }
	}
    }
}

void Landscape_space_statistics::ROut(int numind, ostream &streamout)
{

  int i,j, k, ss, ps, p;
  size_t sz;
  vector <PackedIndividual_space> IVec;
  vector <int> diptbl;

  //find the number of occupied habitats


  IVec.reserve(numind);
  diptbl.reserve(nloc);
  
  for (k=0;k<nloc;k++)
    {
      diptbl.push_back(k);
    }

  ps=0;
  j=0;

  for (i=0;i<nhab;i++)
    {
      if (PopSize(i)>0)
	{
	  j++;
	}
    }

  streamout << "pop class individual locus aindex allele"<<endl;

  for (i=0;i<nhab;i++)
    {
      ps = PopSize(i);
      if (ps>0)
	{
	  IVec.resize(0);
	  for (j=i*s;j<((i*s)+s);j++)
	    {
	      I[j].ResetIndividuals();
	      for (sz=0;sz<I[j].size();sz++)
		{
		  IVec.push_back(I[j].GetCurrentIndividual());
		  I[j].NextIndividual();
		}
	    }
	  random_shuffle(IVec.begin(),IVec.end());


	  if (ps>numind)
	    {
	      ss = numind;
	    }
	  else
	    {
	      ss = ps;
	    }
	  
	  for (j=0;j<ss;j++)
	    {

	      for (sz=0;sz<diptbl.size();sz++)
		{
		  for (p=0;p<Atbls[sz]->getPloidy();p++)
		    {
		      streamout << setw(3) <<i; //population
		      streamout << " " << setw(3) << IVec[j].GetClass();//class
		      streamout << " " << setw(3) << j; //individual
		      streamout << " " << setw(3) << sz;//locus
		      streamout <<" "  << setw(3) << p;//allele index
		      streamout << " " << setw(3) << IVec[j].GetAllele(diptbl[sz],p) ;//allele
		      streamout << endl;
		    }
		}
	    }
	}
    }
}


vector <int>  Landscape_space_statistics::Rmat(int numind)
{
  //numind=0 (default), sample all ind

  int i,j, k, ss, ps, p;
  size_t sz;
  vector <PackedIndividual_space> IVec;
  vector <int> diptbl;
  vector <int> retval;


  //find the number of occupied habitats
  IVec.reserve(numind);
  diptbl.reserve(nloc);
  retval.reserve(2000);

  for (k=0;k<nloc;k++)
    {
      diptbl.push_back(k);
    }

  ps=0;
  j=0;

  for (i=0;i<nhab;i++)
    {
      if (PopSize(i)>0)
	{
	  j++;
	}
    }

  //  streamout << "pop class individual locus aindex allele"<<endl;

  for (i=0;i<nhab;i++)
    {
      ps = PopSize(i);
      if (ps>0)
	{
	  IVec.resize(0);
	  for (j=i*s;j<((i*s)+s);j++)
	    {
	      I[j].ResetIndividuals();
	      for (sz=0;sz<I[j].size();sz++)
		{
		  IVec.push_back(I[j].GetCurrentIndividual());
		  I[j].NextIndividual();
		}
	    }
	  random_shuffle(IVec.begin(),IVec.end());


	  if ((numind>0)&&(ps>numind))
	    {
	      ss = numind;
	    }
	  else
	    {
	      ss = ps;
	    }
	  
	  for (j=0;j<ss;j++)
	    {

	      for (sz=0;sz<diptbl.size();sz++)
		{
		  for (p=0;p<Atbls[sz]->getPloidy();p++)
		    {
		      retval.push_back(i); //population
		      retval.push_back(IVec[j].GetClass());//class
		      retval.push_back(j); //individual
		      retval.push_back(sz);//locus
		      retval.push_back(p);//allele index
		      retval.push_back(IVec[j].GetAllele(diptbl[sz],p)) ;//allele
		    }
		}
	    }
	}
    }
  return retval;
}




/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; minor-mode: font-lock ***
;;; End: ***
*/

