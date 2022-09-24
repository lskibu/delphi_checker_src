# delphi_checker_src
A multi-threaded Win32 bruteforcing software!
I wrote that checker myself when I was active 
on cracking forums. 
With that src, you can easily compile your own
cracking software uing RAD studio (Delphi) by 
justing modifying few lines of code!

### Features 
- Multi-Threaded up to 300 simultanous threads
- Supports Loading proxy from file or remote source
- Load testing lists from File (ansi, utf8...etc)
- Stop/resume checking any time
- A simple UI 
- You can implement your own checking procedure
- You don't have to rewrite a checker from scratch anymore.
- A prior knowledge in Pascal/Delphi is required.


### Disclaimer
I do not take any responsability on how you compile
or use this software. It is uploaded for learning
and educational purposes. 
I am not responsible if you use this software for:
- Performing DoS attack
- Testing a website that you donnot have permission to test
- Credential stuffing

## How to use the source code?
1. Rename files: uUplayBruteProc.pas to uBruteProc.pas , Uplay.dpr to Checker.dpr
2. Edit the contained procedure in uBruteProc.pas
3. Check for necessary modifications in other parts of src such as uThread.pas...
4. Try to compile and check for any syntax errors

## Required packages for compiling that software 
- Ipworks library (SSL/NET/HTTP)
- JCL/JVCL framework library
- Delphi Tokyo 10.2 edition
