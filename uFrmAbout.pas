unit uFrmAbout;

interface

uses WinApi.Windows, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Forms, Vcl.Controls, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls,
  Vcl.Imaging.pngimage;

type
  TAboutBox = class(TForm)
    Panel1: TPanel;
    ProgramIcon: TImage;
    ProductName: TLabel;
    Version: TLabel;
    OKButton: TButton;
    procedure OKButtonClick(Sender: TObject);
    procedure VersionClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutBox: TAboutBox;

implementation

uses ShellApi;

{$R *.dfm}

procedure TAboutBox.OKButtonClick(Sender: TObject);
begin
  Self.Close;
end;

procedure TAboutBox.VersionClick(Sender: TObject);
begin
  ShellExecute(0, 'open', 'https://t.me/DemonTools_1', nil, nil, 0);
end;

end.
