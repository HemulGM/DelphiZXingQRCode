program DelphiZXingQRCodeTestAppFMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  DelphiZXingQRCodeTestAppMainForm in 'DelphiZXingQRCodeTestAppMainForm.pas' {Form5},
  DelphiZXIngQRCode in '..\Source\DelphiZXIngQRCode.pas',
  DelphiZXIngQRCode.FMX in '..\Source\DelphiZXIngQRCode.FMX.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm5, Form5);
  Application.Run;
end.
