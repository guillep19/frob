#include <kernel.h>
#include <osek_kernel.h>
#include <string.h>

typedef void (*COPYFP)();
typedef StatusType (*FILTFP)() ;
#define com_local_memset	memset
#define FILTFP_NULL	NULL
#define FP_NULL	NULL

typedef UINT8 CANType;
typedef FP	ULSndFunc;
typedef FP	ULSndCancelFunc;
typedef FP	ULRcvFunc;

typedef UINT8 IPDUType;

typedef UINT8 TickCounterType;
typedef UINT8 TimerType;
typedef UINT8 TimerTickType;
typedef void (*TIMFP)(FP);
typedef UINT8 LINType;

#define MSG_NULL	((MessageIdentifier)0)



typedef void (*CALLOUTFP)(FP) ;
#define CALLOUTFP_NULL	NULL



