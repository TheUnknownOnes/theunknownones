
/*
 *			The sample of how to make a stream
*/

#ifndef		DEF_MYSTREAM
#define		DEF_MYSTREAM

BOOL		CreateMyMemStream( cdStream*, cdUInt32 );
void		ReleaseMyMemStream( cdStream* );
cdUInt32	GetMyMemStreamSize( cdStream* );
void		CopyMyMemStream( cdStream*, cdStream*, cdUInt32*, cdError* );
void		SaveMyMemStream( cdStream*, cdChar*, cdError* );

BOOL		CreateMyFilStream( cdStream*, cdChar* );
void		ReleaseMyFilStream( cdStream* );


#endif

