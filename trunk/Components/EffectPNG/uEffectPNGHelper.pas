unit uEffectPNGHelper;

interface

uses
  pngimage, uEffectPNG, Classes, Graphics;

  
function PNGtoGlyph(APNG : TPNGObject;
                    const AGlyph : TBitmap;
                    ABackcolor : TColor;
                    AEffectEnabled : TPNGEffects;
                    AEffectDisabled : TPNGEffects = nil;
                    AEffectPressed : TPNGEffects = nil;
                    AEffectChecked : TPNGEffects = nil) : Integer;

implementation

function PNGtoGlyph(APNG : TPNGObject;
                    const AGlyph : TBitmap;
                    ABackcolor : TColor;
                    AEffectEnabled : TPNGEffects;
                    AEffectDisabled : TPNGEffects = nil;
                    AEffectPressed : TPNGEffects = nil;
                    AEffectChecked : TPNGEffects = nil) : Integer;
                    
  procedure DrawPNG(const APNGToDraw : TPNGObject; AIndex : Integer);
  begin
    APNGToDraw.Draw(AGlyph.Canvas, Rect(APNG.Width * AIndex,
                                        0,
                                        APNG.Width * AIndex + APNGToDraw.Width,
                                        APNG.Height));
  end;
var
  TempPNG : TPNGObject;
begin
  Result := 1;
  if Assigned(AEffectDisabled) then Inc(Result);
  if Assigned(AEffectPressed) then Inc(Result);
  if Assigned(AEffectChecked) then Inc(Result);

  AGlyph.Width := APNG.Width * Result;
  AGlyph.Height := APNG.Height;

  AGlyph.Canvas.Brush.Color := ABackcolor;
  AGlyph.Canvas.Brush.Style := bsSolid;
  AGlyph.Canvas.FillRect(AGlyph.Canvas.ClipRect);
  
  TempPNG := TPNGObject.Create;
  try
    TempPNG.Assign(APNG);
    AEffectEnabled.ApplyEffects(TempPNG);
    DrawPNG(TempPNG, 0);

    if Assigned(AEffectDisabled) then
    begin
      TempPNG.Assign(APNG);
      AEffectDisabled.ApplyEffects(TempPNG);
      DrawPNG(TempPNG, 1);
    end;

    if Assigned(AEffectPressed) then
    begin
      TempPNG.Assign(APNG);
      AEffectPressed.ApplyEffects(TempPNG);
      DrawPNG(TempPNG, 2);
    end;

    if Assigned(AEffectChecked) then
    begin
      TempPNG.Assign(APNG);
      AEffectChecked.ApplyEffects(TempPNG);
      DrawPNG(TempPNG, 3);
    end;
  finally
    TempPNG.Free;
  end;
  
end;

end.
