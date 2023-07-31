unit DelphiZXIngQRCode.FMX;

interface

uses
  FMX.Graphics, System.UITypes, DelphiZXIngQRCode;

{$SCOPEDENUMS ON}

type
  TQRKind = (Default, Square, Rounded, Circle, Lines);

  TZXingQRCodeDraw = class helper for TZXingQRCode
    function GetBitmap(const Size: Integer; Color: TAlphaColor = TAlphaColors.Black; Background: TAlphaColor = TAlphaColors.Null; Kind: TQRKind = TQRKind.Default): TBitmap;
    function GetPath(Kind: TQRKind = TQRKind.Default): string;
  end;

implementation

uses
  System.Types, FMX.Types, FMX.Objects;

{ TZXingQRCodeDraw }

function TZXingQRCodeDraw.GetBitmap(const Size: Integer; Color, Background: TAlphaColor; Kind: TQRKind): TBitmap;
begin
  Result := TBitmap.Create;
  try
    var BitSize := Trunc(Size / Columns);
    Result.SetSize(Rows * BitSize, Columns * BitSize);
    Result.Canvas.BeginScene;
    with Result.Canvas do
    try
      Clear(Background);
      for var Row := 0 to Rows - 1 do
        for var Column := 0 to Columns - 1 do
        begin
          if IsBlack[Row, Column] then
          begin
            Fill.Color := Color;
            Fill.Kind := TBrushKind.Solid;
            var BitRect: TRectF;
            BitRect.Left := Column * BitSize;
            BitRect.Top := Row * BitSize;
            BitRect.Width := BitSize;
            BitRect.Height := BitSize;
            case Kind of
              TQRKind.Default, TQRKind.Square:
                FillRect(BitRect, 0, 0, AllCorners, 1);
              TQRKind.Circle:
                FillRect(BitRect, BitSize / 2, BitSize / 2, AllCorners, 1);
              TQRKind.Lines:
                begin
                  var Corners: TCorners := [];
                  if not IsBlack[Row, Column - 1] then
                    Corners := Corners + [TCorner.TopLeft, TCorner.BottomLeft];
                  if not IsBlack[Row, Column + 1] then
                    Corners := Corners + [TCorner.TopRight, TCorner.BottomRight];
                  FillRect(BitRect, BitSize / 2, BitSize / 2, Corners, 1);
                end;
              TQRKind.Rounded:
                begin
                  var Corners: TCorners := AllCorners;
                  if IsBlack[Row, Column - 1] then
                    Corners := Corners - [TCorner.TopLeft, TCorner.BottomLeft];
                  if IsBlack[Row, Column + 1] then
                    Corners := Corners - [TCorner.TopRight, TCorner.BottomRight];
                  if IsBlack[Row - 1, Column] then
                    Corners := Corners - [TCorner.TopRight, TCorner.TopLeft];
                  if IsBlack[Row + 1, Column] then
                    Corners := Corners - [TCorner.BottomLeft, TCorner.BottomRight];
                  FillRect(BitRect, BitSize / 2, BitSize / 2, Corners, 1);
                end;
            end;
          end;
        end;
    finally
      Result.Canvas.EndScene;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TZXingQRCodeDraw.GetPath(Kind: TQRKind): string;
begin
  var Path := TPathData.Create;
  var Size := 500;
  Result := '';
  try
    var BitSize := Trunc(Size / Columns);
    for var Row := 0 to Rows - 1 do
      for var Column := 0 to Columns - 1 do
      begin
        if IsBlack[Row, Column] then
        begin
          var BitRect: TRectF;
          BitRect.Left := Column * BitSize;
          BitRect.Top := Row * BitSize;
          BitRect.Width := BitSize;
          BitRect.Height := BitSize;
          case Kind of
            TQRKind.Default, TQRKind.Square:
              Path.AddRectangle(BitRect, 0, 0, AllCorners);
            TQRKind.Circle:
              Path.AddRectangle(BitRect, BitSize / 2, BitSize / 2, AllCorners);
            TQRKind.Lines:
              begin
                var Corners: TCorners := [];
                if not IsBlack[Row, Column - 1] then
                  Corners := Corners + [TCorner.TopLeft, TCorner.BottomLeft];
                if not IsBlack[Row, Column + 1] then
                  Corners := Corners + [TCorner.TopRight, TCorner.BottomRight];
                Path.AddRectangle(BitRect, BitSize / 2, BitSize / 2, Corners);
              end;
            TQRKind.Rounded:
              begin
                var Corners: TCorners := AllCorners;
                if IsBlack[Row, Column - 1] then
                  Corners := Corners - [TCorner.TopLeft, TCorner.BottomLeft];
                if IsBlack[Row, Column + 1] then
                  Corners := Corners - [TCorner.TopRight, TCorner.BottomRight];
                if IsBlack[Row - 1, Column] then
                  Corners := Corners - [TCorner.TopRight, TCorner.TopLeft];
                if IsBlack[Row + 1, Column] then
                  Corners := Corners - [TCorner.BottomLeft, TCorner.BottomRight];
                Path.AddRectangle(BitRect, BitSize / 2, BitSize / 2, Corners);
              end;
          end;
        end;
      end;
    Result := Path.Data;
  except
    raise;
  end;
end;

end.

