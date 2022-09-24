unit uUplayBruteProc;

interface

uses
  Classes,
  uTypes,
  SysUtils,
  ipwhttp,
  RegularExpressions,
  ipwJSON;

procedure UplayBruteProc(Http: TipwHTTP; const Username, Password: string;
  var Status: TStatus; Capture: TStringList);

implementation

procedure UplayBruteProc(Http: TipwHTTP; const Username, Password: string;
  var Status: TStatus; Capture: TStringList);
begin
{ 
    left empty inorder to not expose
    some private API infos
    
    That proc is responsible for checking the 
    validity of supplied Username & Password Arguments

    if you want to check SMTP/SSH or other protocols
    You will need to replace Http: TipwHTTP with
    desired Client Type/Class


    example
    Http.PostData = Username + Password;
    Http.Post('http://example.com/');

    if http.ResonseData.contains("error") then
	Status := Failed;
}
end;

end.
