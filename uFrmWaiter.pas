unit uFrmWaiter;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, JvExControls, JvWaitingGradient;

type
  TFrmWaiter = class(TForm)
    JvWaitingGradient1: TJvWaitingGradient;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmWaiter: TFrmWaiter;

implementation

{$R *.dfm}

end.
