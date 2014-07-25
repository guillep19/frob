fastThread . setThreadFunction ( test_fast ) = SUCCESS
recursiveThread . setThreadFunction ( test_recursive ) = SUCCESS
errorThread . setThreadFunction ( test_error ) = SUCCESS
fastMutex = Mutex:  0x........   (Unlocked)
fastMutex . lock() = SUCCESS
fastMutex = Mutex:  0x........   (Locked)
fastMutex . unlock() = SUCCESS
fastMutex = Mutex:  0x........   (Unlocked)


recursiveMutex = Mutex:  0x........   (Unlocked)
recursiveMutex . lock() = SUCCESS
Recursive Mutexes only register as being locked if they are locked by a different Thread.
recursiveMutex = Mutex:  0x........   (Unlocked)
recursiveMutex . lock() = SUCCESS
recursiveMutex . unlock() = SUCCESS
recursiveMutex . unlock() = SUCCESS
recursiveMutex = Mutex:  0x........   (Unlocked)


errorMutex = Mutex:  0x........   (Unlocked)
errorMutex . lock() = SUCCESS
errorMutex = Mutex:  0x........   (Locked)
errorMutex . lock() = [Mutex:lock]  Error:  [EDEADLK]  Unable to lock mutex:  Mutex already locked by calling thread.
FAILURE
errorMutex . unlock() = SUCCESS
errorMutex = Mutex:  0x........   (Unlocked)


FastMutex Thread Running
fastMutex.lock() = SUCCESS
fastMutex = Mutex:  0x........   (Locked)
FastMutex Thread Ending after unlock
fastMutex.unlock() = SUCCESS
fastMutex . lock() = SUCCESS
fastMutex = Mutex:  0x........   (Locked)
fastMutex . unlock() = SUCCESS
fastMutex = Mutex:  0x........   (Unlocked)
fastThread . waitForThreadToStop() = SUCCESS


recursiveMutex Thread Running
recursiveMutex.lock() = SUCCESS
recursiveMutex = Mutex:  0x........   (Locked)
recursiveMutex Thread Ending after unlock
recursiveMutex.unlock() = SUCCESS
recursiveMutex . lock() = SUCCESS
Recursive Mutexes only register as being locked if they are locked by a different Thread.
recursiveMutex = Mutex:  0x........   (Unlocked)
recursiveMutex . lock() = SUCCESS
recursiveMutex . unlock() = SUCCESS
recursiveMutex . unlock() = SUCCESS
recursiveMutex = Mutex:  0x........   (Unlocked)
recursiveThread . waitForThreadToStop() = SUCCESS


errorMutex Thread Running
errorMutex.lock() = SUCCESS
errorMutex = Mutex:  0x........   (Locked)
errorMutex Thread Ending after unlock
errorMutex.unlock() = SUCCESS
errorMutex . lock() = SUCCESS
errorMutex = Mutex:  0x........   (Locked)
errorMutex . lock() = [Mutex:lock]  Error:  [EDEADLK]  Unable to lock mutex:  Mutex already locked by calling thread.
FAILURE
errorMutex . unlock() = SUCCESS
errorMutex = Mutex:  0x........   (Unlocked)
errorThread . waitForThreadToStop() = SUCCESS
