unit DelphiZXIngQRCode.Vcl;

interface

uses
  Vcl.Graphics, System.UITypes, DelphiZXIngQRCode;

{$SCOPEDENUMS ON}

type
  TZXingQRCodeDraw = class helper for TZXingQRCode
    function GetBitmap(Color: TColor = TColors.Black; Background: TColor = TColors.Null): TBitmap;
  end;

implementation

{ TZXingQRCodeDraw }

function TZXingQRCodeDraw.GetBitmap(Color, Background: TColor): TBitmap;
var
  Row, Column: Integer;
begin
  Result := TBitmap.Create;
  try
    Result.SetSize(Rows, Columns);
    for Row := 0 to Rows - 1 do
      for Column := 0 to Columns - 1 do
        if (IsBlack[Row, Column]) then
          Result.Canvas.Pixels[Column, Row] := Color
        else
          Result.Canvas.Pixels[Column, Row] := Background;
  except
    Result.Free;
    raise;
  end;
end;

end.

