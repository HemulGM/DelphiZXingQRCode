unit DelphiZXingQRCodeTestAppMainForm;

// Demo app for ZXing QRCode port to Delphi, by Debenu Pty Ltd (www.debenu.com)
// Need a PDF SDK? Checkout Debenu Quick PDF Library: http://www.debenu.com/products/development/debenu-pdf-library/

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  DelphiZXingQRCode, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    edtText: TEdit;
    Label1: TLabel;
    cmbEncoding: TComboBox;
    Label2: TLabel;
    Label3: TLabel;
    edtQuietZone: TEdit;
    Label4: TLabel;
    PaintBox1: TPaintBox;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure edtTextChange(Sender: TObject);
    procedure cmbEncodingChange(Sender: TObject);
    procedure edtQuietZoneChange(Sender: TObject);
  private
    QRCodeBitmap: TBitmap;
  public
    procedure UpdateQR;
  end;

var
  Form1: TForm1;

implementation

uses
  DelphiZXIngQRCode.Vcl;

{$R *.dfm}

procedure TForm1.cmbEncodingChange(Sender: TObject);
begin
  UpdateQR;
end;

procedure TForm1.edtQuietZoneChange(Sender: TObject);
begin
  UpdateQR;
end;

procedure TForm1.edtTextChange(Sender: TObject);
begin
  UpdateQR;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  QRCodeBitmap := TBitmap.Create;
  UpdateQR;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  QRCodeBitmap.Free;
end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
var
  Scale: Double;
begin
  PaintBox1.Canvas.Brush.Color := clWhite;
  PaintBox1.Canvas.FillRect(Rect(0, 0, PaintBox1.Width, PaintBox1.Height));
  if ((QRCodeBitmap.Width > 0) and (QRCodeBitmap.Height > 0)) then
  begin
    if (PaintBox1.Width < PaintBox1.Height) then
      Scale := PaintBox1.Width / QRCodeBitmap.Width
    else
      Scale := PaintBox1.Height / QRCodeBitmap.Height;
    PaintBox1.Canvas.StretchDraw(Rect(0, 0, Trunc(Scale * QRCodeBitmap.Width), Trunc(Scale * QRCodeBitmap.Height)), QRCodeBitmap);
  end;
end;

procedure TForm1.UpdateQR;
var
  QRCode: TZXingQRCode;
begin
  QRCode := TZXingQRCode.Create;
  try
    QRCode.BeginUpdate;
    try
      QRCode.Data := edtText.Text;
      QRCode.Encoding := TQRCodeEncoding(cmbEncoding.ItemIndex);
      QRCode.QuietZone := StrToIntDef(edtQuietZone.Text, 4);
    finally
      QRCode.EndUpdate;
    end;

    //Image
    var Bitmap := QRCode.GetBitmap(clBlack, clWhite);
    try
      QRCodeBitmap.Assign(Bitmap);
    finally
      Bitmap.Free;
    end;
  finally
    QRCode.Free;
  end;
  PaintBox1.Repaint;
end;

end.

