program DelphiZXingQRCodeTestApp;

uses
  Vcl.Forms,
  DelphiZXingQRCodeTestAppMainForm in 'DelphiZXingQRCodeTestAppMainForm.pas' {Form1},
  DelphiZXIngQRCode.Vcl in '..\Source\DelphiZXIngQRCode.Vcl.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
