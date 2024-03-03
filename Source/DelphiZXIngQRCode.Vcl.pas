unit DelphiZXIngQRCode.Vcl;

interface

uses
  Vcl.Graphics, System.Types, System.UITypes, DelphiZXIngQRCode;

{$SCOPEDENUMS ON}

type
  TQRKind = (Default, Square, Rounded, Circle, Lines);

  TZXingQRCodeDraw = class helper for TZXingQRCode
    function GetBitmap(Color: TColor = TColors.Black; Background: TColor = TColors.Null): TBitmap; overload;
    function GetBitmap(Size: Integer; Kind: TQRKind = TQRKind.Default; Color: TColor = TColors.Black; Background: TColor = TColors.Null): TBitmap; overload;
  end;

implementation

uses
  System.Math, Vcl.Direct2D;

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

function TZXingQRCodeDraw.GetBitmap(Size: Integer; Kind: TQRKind; Color, Background: TColor): TBitmap;
var
  Row, Column: Integer;
begin
  Result := TBitmap.Create(Size, Size);
  try
    Result.PixelFormat := pf32bit;
    with TDirect2DCanvas.Create(Result.Canvas, Rect(0, 0, Size, Size)) do
    try
      BeginDraw;
      Brush.Color := Background;
      FillRect(Rect(0, 0, Size, Size));
      var BitSize := Min(Trunc(Size / Rows), Trunc(Size / Columns));
      for Row := 0 to Rows - 1 do
        for Column := 0 to Columns - 1 do
          if IsBlack[Row, Column] then
          begin
            var BitRect := TRect.Create(TPoint.Create(Row * BitSize, Column * BitSize), BitSize, BitSize);
            Brush.Color := Color;
            //RoundRect(BitRect, 8, 8);

            case Kind of
              TQRKind.Default, TQRKind.Square:
                FillRect(BitRect);
              TQRKind.Circle:
                RoundRect(BitRect, BitSize, BitSize);
              TQRKind.Lines:
                begin
                 { var Corners: TCorners := [];
                  if not IsBlack[Row, Column - 1] then
                    Corners := Corners + [TCorner.TopLeft, TCorner.BottomLeft];
                  if not IsBlack[Row, Column + 1] then
                    Corners := Corners + [TCorner.TopRight, TCorner.BottomRight];
                  FillRect(BitRect, BitSize / 2, BitSize / 2, Corners, 1);  }
                end;
              TQRKind.Rounded:
                begin
                 { var Corners: TCorners := AllCorners;
                  if IsBlack[Row, Column - 1] then
                    Corners := Corners - [TCorner.TopLeft, TCorner.BottomLeft];
                  if IsBlack[Row, Column + 1] then
                    Corners := Corners - [TCorner.TopRight, TCorner.BottomRight];
                  if IsBlack[Row - 1, Column] then
                    Corners := Corners - [TCorner.TopRight, TCorner.TopLeft];
                  if IsBlack[Row + 1, Column] then
                    Corners := Corners - [TCorner.BottomLeft, TCorner.BottomRight];
                  FillRect(BitRect, BitSize / 2, BitSize / 2, Corners, 1);  }
                end;
            end;
          end;
    finally
      EndDraw;
    end;
  except
    Result.Free;
    raise;
  end;
end;

end.

