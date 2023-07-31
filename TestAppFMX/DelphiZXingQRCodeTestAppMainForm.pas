unit DelphiZXingQRCodeTestAppMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  DelphiZXIngQRCode, FMX.ListBox, FMX.Edit, FMX.Objects,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.EditBox, FMX.SpinBox, FMX.Colors,
  FMX.Layouts;

type
  TForm5 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    edtText: TEdit;
    cmbEncoding: TComboBox;
    edtQuietZone: TEdit;
    Label6: TLabel;
    ComboColorBoxBG: TComboColorBox;
    Label7: TLabel;
    ComboColorBoxColor: TComboColorBox;
    Button1: TButton;
    Label8: TLabel;
    ComboBoxKind: TComboBox;
    Rectangle1: TRectangle;
    ImageQRMini: TImage;
    Path1: TPath;
    Path2: TPath;
    Label9: TLabel;
    ComboBoxErrorCorrection: TComboBox;
    GridPanelLayout1: TGridPanelLayout;
    PathQR: TPath;
    Layout1: TLayout;
    ImageQR: TImage;
    Layout2: TLayout;
    Label5: TLabel;
    SpinBoxSize: TSpinBox;
    CheckBoxAutoSize: TCheckBox;
    procedure edtTextChangeTracking(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure UpdateQR;
  public
  end;

var
  Form5: TForm5;

implementation

uses
  DelphiZXIngQRCode.FMX, System.Math;

{$R *.fmx}

procedure TForm5.edtTextChangeTracking(Sender: TObject);
begin
  UpdateQR;
end;

procedure TForm5.FormCreate(Sender: TObject);
begin
  UpdateQR;
end;

procedure TForm5.UpdateQR;
var
  QRCode: TZXingQRCode;
begin
  QRCode := TZXingQRCode.Create;
  try
    QRCode.BeginUpdate;
    try
      QRCode.Data := edtText.Text;
      QRCode.Encoding := TQRCodeEncoding(cmbEncoding.ItemIndex);
      QRCode.ErrorCorrectionLevel := TQRErrorCorrectionLevel(ComboBoxErrorCorrection.ItemIndex);
      QRCode.QuietZone := StrToIntDef(edtQuietZone.Text, 4);
    finally
      QRCode.EndUpdate;
    end;

    //Image
    var Size: Integer;
    if CheckBoxAutoSize.IsChecked then
      Size := Trunc(Min(ImageQR.Width, ImageQR.Height))
    else
      Size := Trunc(SpinBoxSize.Value);
    var Bitmap: TBitmap := QRCode.GetBitmap(Size, ComboColorBoxColor.Color, ComboColorBoxBG.Color, TQRKind(ComboBoxKind.ItemIndex));
    try
      ImageQR.Bitmap.Assign(Bitmap);
    finally
      Bitmap.Free;
    end;

    // Image mini
    Bitmap := QRCode.GetBitmap(Trunc(Min(ImageQRMini.Width, ImageQRMini.Height)), ComboColorBoxColor.Color, ComboColorBoxBG.Color, TQRKind(ComboBoxKind.ItemIndex));
    try
      ImageQRMini.Bitmap.Assign(Bitmap);
    finally
      Bitmap.Free;
    end;

    //Path
    PathQR.Fill.Color := ComboColorBoxColor.Color;
    PathQR.Data.Data := QRCode.GetPath(TQRKind(ComboBoxKind.ItemIndex));
  finally
    QRCode.Free;
  end;
end;

end.

