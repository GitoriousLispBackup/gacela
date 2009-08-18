
#include "cmpinclude.h"
#include "gacela_chip.h"
void init__home_jsancho_proyectos_gacela_gacela_chip(){do_init((void *)VV);}
#include "gacela_chipmunk.c"
/*	function definition for CPINITCHIPMUNK	*/

static void L1()
{	object *old_base=vs_base;
	gacela_cpInitChipmunk();
	vs_top=(vs_base=old_base)+1;
	vs_base[0]=Cnil;
}
/*	function definition for CPRESETSHAPEIDCOUNTER	*/

static void L2()
{	object *old_base=vs_base;
	gacela_cpResetShapeIdCounter();
	vs_top=(vs_base=old_base)+1;
	vs_base[0]=Cnil;
}
/*	function definition for CPSPACENEW	*/

static void L3()
{	object *old_base=vs_base;
	int x;
	x=
	gacela_cpSpaceNew();
	vs_top=(vs_base=old_base)+1;
	vs_base[0]=CMPmake_fixnum(x);
}
/*	function definition for CPSPACEADDBODY	*/

static void L4()
{	object *old_base=vs_base;
	gacela_cpSpaceAddBody(
	object_to_int(vs_base[0]),
	object_to_int(vs_base[1]));
	vs_top=(vs_base=old_base)+1;
	vs_base[0]=Cnil;
}
/*	function definition for CPSPACEADDSHAPE	*/

static void L5()
{	object *old_base=vs_base;
	gacela_cpSpaceAddShape(
	object_to_int(vs_base[0]),
	object_to_int(vs_base[1]));
	vs_top=(vs_base=old_base)+1;
	vs_base[0]=Cnil;
}
/*	function definition for CPSPACEFREE	*/

static void L6()
{	object *old_base=vs_base;
	gacela_cpSpaceFree(
	object_to_int(vs_base[0]));
	vs_top=(vs_base=old_base)+1;
	vs_base[0]=Cnil;
}
/*	function definition for CPBODYNEW	*/

static void L7()
{	object *old_base=vs_base;
	int x;
	x=
	gacela_cpBodyNew(
	object_to_float(vs_base[0]),
	object_to_float(vs_base[1]),
	object_to_float(vs_base[2]));
	vs_top=(vs_base=old_base)+1;
	vs_base[0]=CMPmake_fixnum(x);
}
/*	function definition for CPBODYFREE	*/

static void L8()
{	object *old_base=vs_base;
	gacela_cpBodyFree(
	object_to_int(vs_base[0]));
	vs_top=(vs_base=old_base)+1;
	vs_base[0]=Cnil;
}
/*	function definition for CPCIRCLESHAPENEW	*/

static void L9()
{	object *old_base=vs_base;
	int x;
	x=
	gacela_cpCircleShapeNew(
	object_to_int(vs_base[0]),
	object_to_float(vs_base[1]),
	object_to_float(vs_base[2]),
	object_to_float(vs_base[3]));
	vs_top=(vs_base=old_base)+1;
	vs_base[0]=CMPmake_fixnum(x);
}
/*	function definition for CPPOLYSHAPENEW	*/

static void L10()
{	object *old_base=vs_base;
	int x;
	x=
	gacela_cpPolyShapeNew(
	object_to_int(vs_base[0]),
	object_to_int(vs_base[1]),
	object_to_int(vs_base[2]),
	object_to_float(vs_base[3]),
	object_to_float(vs_base[4]));
	vs_top=(vs_base=old_base)+1;
	vs_base[0]=CMPmake_fixnum(x);
}
/*	function definition for CPSHAPEFREE	*/

static void L11()
{	object *old_base=vs_base;
	gacela_cpShapeFree(
	object_to_int(vs_base[0]));
	vs_top=(vs_base=old_base)+1;
	vs_base[0]=Cnil;
}
/*	function definition for SET-SPACE-PROPERTIES	*/

static void L12()
{	object *old_base=vs_base;
	set_space_properties(
	object_to_int(vs_base[0]),
	object_to_float(vs_base[1]),
	object_to_float(vs_base[2]));
	vs_top=(vs_base=old_base)+1;
	vs_base[0]=Cnil;
}
/*	function definition for MAKE-SPACE	*/

static void L13()
{register object *base=vs_base;
	register object *sup=base+VM1; VC1
	vs_check;
	{object V1;
	parse_key(vs_base,FALSE,FALSE,1,VV[3]);vs_top=sup;
	V1=(base[0]);
	base[2]= ((object)VV[0]);
	base[3]= (V1);
	vs_top=(vs_base=base+2)+2;
	siLmake_structure();
	return;
	}
}
/*	function definition for MAKE-BODY	*/

static void L14()
{register object *base=vs_base;
	register object *sup=base+VM2; VC2
	vs_check;
	{object V2;
	parse_key(vs_base,FALSE,FALSE,1,VV[3]);vs_top=sup;
	V2=(base[0]);
	base[2]= ((object)VV[1]);
	base[3]= (V2);
	vs_top=(vs_base=base+2)+2;
	siLmake_structure();
	return;
	}
}
/*	function definition for MAKE-SHAPE	*/

static void L15()
{register object *base=vs_base;
	register object *sup=base+VM3; VC3
	vs_check;
	{object V3;
	parse_key(vs_base,FALSE,FALSE,1,VV[3]);vs_top=sup;
	V3=(base[0]);
	base[2]= ((object)VV[2]);
	base[3]= (V3);
	vs_top=(vs_base=base+2)+2;
	siLmake_structure();
	return;
	}
}
/*	function definition for CREATE-SPACE	*/

static void L16()
{register object *base=vs_base;
	register object *sup=base+VM4; VC4
	vs_check;
	{object V4;
	parse_key(vs_base,FALSE,FALSE,1,VV[7]);vs_top=sup;
	V4=(base[0]);
	vs_base=vs_top;
	(void) (*Lnk8)();
	vs_top=sup;
	{object V5;
	register object V6;
	base[2]= ((object)VV[3]);
	vs_base=vs_top;
	(void) (*Lnk9)();
	vs_top=sup;
	base[3]= vs_base[0];
	vs_top=(vs_base=base+2)+2;
	(void) (*Lnk10)();
	vs_top=sup;
	V5= vs_base[0];
	V6= Cnil;
	base[2]= ((object)VV[0]);
	base[3]= (V5);
	vs_base=vs_top;
	Lgentemp();
	vs_top=sup;
	base[4]= vs_base[0];
	vs_top=(vs_base=base+2)+3;
	(void) (*Lnk11)();
	vs_top=sup;
	if(((V4))==Cnil){
	goto T15;}
	V6= (VFUN_NARGS=2,(*(LnkLI12))((V4),(V6)));
	goto T15;
T15:;
	if(((V6))==Cnil){
	goto T19;}
	{object V7;
	V7= make_cons(STREF(object,(V5),0),(V6));
	 vs_top=base+2;
	 while(V7!=Cnil)
	 {vs_push((V7)->c.c_car);V7=(V7)->c.c_cdr;}
	vs_base=base+2;}
	(void) (*Lnk13)();
	vs_top=sup;
	goto T19;
T19:;
	base[2]= (V5);
	vs_top=(vs_base=base+2)+1;
	return;}
	}
}
/*	function definition for CREATE-BODY	*/

static void L17()
{register object *base=vs_base;
	register object *sup=base+VM5; VC5
	vs_check;
	{object V8;
	object V9;
	parse_key(vs_base,FALSE,FALSE,2,VV[14],VV[15]);vs_top=sup;
	if(base[2]==Cnil){
	V8= ((object)VV[4]);
	}else{
	V8=(base[0]);}
	if(base[3]==Cnil){
	V9= ((object)VV[5]);
	}else{
	V9=(base[1]);}
	vs_base=vs_top;
	(void) (*Lnk8)();
	vs_top=sup;
	{object V10;
	base[4]= ((object)VV[3]);
	base[6]= (V8);
	base[7]= (V9);
	base[8]= ((object)VV[6]);
	vs_top=(vs_base=base+6)+3;
	(void) (*Lnk16)();
	vs_top=sup;
	base[5]= vs_base[0];
	vs_top=(vs_base=base+4)+2;
	(void) (*Lnk17)();
	vs_top=sup;
	V10= vs_base[0];
	base[4]= ((object)VV[1]);
	base[5]= (V10);
	vs_base=vs_top;
	Lgentemp();
	vs_top=sup;
	base[6]= vs_base[0];
	vs_top=(vs_base=base+4)+3;
	(void) (*Lnk11)();
	vs_top=sup;
	base[4]= (V10);
	vs_top=(vs_base=base+4)+1;
	return;}
	}
}
static void LnkT17(){ call_or_link(((object)VV[17]),(void **)(void *)&Lnk17);} /* MAKE-BODY */
static void LnkT16(){ call_or_link(((object)VV[16]),(void **)(void *)&Lnk16);} /* CPNEWBODY */
static void LnkT13(){ call_or_link(((object)VV[13]),(void **)(void *)&Lnk13);} /* SET-SPACE-PROPERTIES */
static object  LnkTLI12(object first,...){object V1;va_list ap;va_start(ap,first);V1=call_vproc_new(((object)VV[12]),(void **)(void *)&LnkLI12,first,ap);va_end(ap);return V1;} /* UNION */
static void LnkT11(){ call_or_link(((object)VV[11]),(void **)(void *)&Lnk11);} /* SET-RESOURCE */
static void LnkT10(){ call_or_link(((object)VV[10]),(void **)(void *)&Lnk10);} /* MAKE-SPACE */
static void LnkT9(){ call_or_link(((object)VV[9]),(void **)(void *)&Lnk9);} /* CPSPACENEW */
static void LnkT8(){ call_or_link(((object)VV[8]),(void **)(void *)&Lnk8);} /* INIT-CHIPMUNK */

#ifdef SYSTEM_SPECIAL_INIT
SYSTEM_SPECIAL_INIT
#endif

