//**********************************************************
// Developed by TheUnkownOnes.net
//
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit uGraphicalTools;

INTERFACE

  USES
    Windows,   // TRGBTriple
    Graphics;  // TBitmap


  CONST
    MaxPixelCount = 65536;

  TYPE
    TReal = DOUBLE;

    // For pf24bit Scanlines
    pRGBTripleArray = ^TRGBTripleArray;
    TRGBTripleArray = ARRAY[0..MaxPixelCount-1] OF TRGBTriple;

  FUNCTION RGBtoRGBTriple(CONST red, green, blue:  BYTE):  TRGBTriple;

  // Integer Conversion Routines
  FUNCTION HSVtoRGBTriple (CONST H,S,V:  INTEGER):  TRGBTriple;
  PROCEDURE RGBTripleToHSV (CONST RGBTriple: TRGBTriple;  {r, g and b IN [0..255]}
                            VAR   H,S,V:  INTEGER);    {h IN 0..359; s,v IN 0..255}

  // Floating Point Conversion Routines
  PROCEDURE HSVtoRGB(CONST H,S,V:  TReal; VAR R,G,B:  TReal);
  PROCEDURE RGBToHSV(CONST R,G,B:  TReal; VAR H,S,V:  TReal);

  // Hue-Saturation Circle
  FUNCTION CreateHueSaturationCircle(CONST size:  INTEGER;
                                     CONST HueLevel:  INTEGER;
                                     CONST SaturationLevel:  INTEGER;
                                     CONST ValueLevel:  INTEGER;
                                     CONST BackgroundColor:  TColor;
                                     CONST SaturationCircleColor:  TColor;
                                     CONST HueLineColor:  TColor):  TBitmap;
                                    



IMPLEMENTATION

  USES
    Math,      // MaxValue
    SysUtils;  // Exception

  TYPE
    EColorError = CLASS(Exception);


  ////////////////////////////////////////////////////////////////////////////

  FUNCTION RGBtoRGBTriple(CONST red, green, blue:  BYTE):  TRGBTriple;
  BEGIN
    WITH RESULT DO
    BEGIN
      rgbtRed   := red;
      rgbtGreen := green;
      rgbtBlue  := blue
    END
  END {RGBTriple};


  ////////////////////////////////////////////////////////////////////////////
  // Integer Routines

  // Floating point fractions, 0..1, replaced with integer values, 0..255.
  // Use integer conversion ONLY for one-way, or a single final conversions.
  // Use floating-point for converting reversibly.
  //
  //  H = 0 to 360 (corresponding to 0..360 degrees around hexcone)
  //      0 (undefined) for S = 0
  //  S = 0 (shade of gray) to 255 (pure color)
  //  V = 0 (black) to 255 (white)

  FUNCTION HSVtoRGBTriple (CONST H,S,V:  INTEGER):  TRGBTriple;
    CONST
      divisor:  INTEGER = 255*60;
    VAR
      f    :  INTEGER;
      hTemp:  INTEGER;
      p,q,t:  INTEGER;
      VS   :  INTEGER;
  BEGIN
    IF   S = 0
    THEN RESULT := RGBtoRGBTriple(V, V, V)  // achromatic:  shades of gray
    ELSE BEGIN                              // chromatic color
      IF   H = 360
      THEN hTemp := 0
      ELSE hTemp := H;

      f     := hTemp MOD 60;     // f is IN [0, 59]
      hTemp := hTemp DIV 60;     // h is now IN [0,6)

      VS := V*S;
      p := V - VS DIV 255;                 // p = v * (1 - s)
      q := V - (VS*f) DIV divisor;         // q = v * (1 - s*f)
      t := V - (VS*(60 - f)) DIV divisor;  // t = v * (1 - s * (1 - f))

      CASE hTemp OF
        0:   RESULT := RGBtoRGBTriple(V, t, p);
        1:   RESULT := RGBtoRGBTriple(q, V, p);
        2:   RESULT := RGBtoRGBTriple(p, V, t);
        3:   RESULT := RGBtoRGBTriple(p, q, V);
        4:   RESULT := RGBtoRGBTriple(t, p, V);
        5:   RESULT := RGBtoRGBTriple(V, p, q);
        ELSE RESULT := RGBtoRGBTriple(0,0,0)  // should never happen;
                                              // avoid compiler warning
      END
    END
  END {HSVtoRGBTriple};


   // RGB, each 0 to 255, to HSV.
  //   H = 0 to 360 (corresponding to 0..360 degrees around hexcone)
  //   S = 0 (shade of gray) to 255 (pure color)
  //   V = 0 (black) to 255 {white)

  // Based on C Code in "Computer Graphics -- Principles and Practice,"
  // Foley et al, 1996, p. 592.  Floating point fractions, 0..1, replaced with
  // integer values, 0..255.

  PROCEDURE RGBTripleToHSV (CONST RGBTriple: TRGBTriple;  {r, g and b IN [0..255]}
                            VAR   H,S,V:  INTEGER);    {h IN 0..359; s,v IN 0..255}
    VAR
      Delta:  INTEGER;
      Min  :  INTEGER;
  BEGIN
    WITH RGBTriple DO
    BEGIN
      Min := MinIntValue( [rgbtRed, rgbtGreen, rgbtBlue] );
      V   := MaxIntValue( [rgbtRed, rgbtGreen, rgbtBlue] )
    END;

    Delta := V - Min;

    // Calculate saturation:  saturation is 0 if r, g and b are all 0
    IF   V =  0
    THEN S := 0
    ELSE S := MulDiv(Delta, 255, V);

    IF   S  = 0
    THEN H := 0   // Achromatic:  When s = 0, h is undefined but assigned the value 0
    ELSE BEGIN    // Chromatic

      WITH RGBTriple DO
      BEGIN
        IF   rgbtRed = V
        THEN  // degrees -- between yellow and magenta
              H := MulDiv(rgbtGreen - rgbtBlue, 60, Delta)
        ELSE
          IF   rgbtGreen = V
          THEN // between cyan and yellow
               H := 120 + MulDiv(rgbtBlue-rgbtRed, 60, Delta)
          ELSE
            IF  rgbtBlue = V
            THEN // between magenta and cyan
                 H := 240 + MulDiv(rgbtRed-rgbtGreen, 60, Delta);
      END;

      IF   H < 0
      THEN H := H + 360;

    END
  END {RGBTripleToHSV};


  ///////////////////////////////////////////////////////////////////////////
  // Floating-Point Routines

  // Based on C Code in "Computer Graphics -- Principles and Practice,"
  // Foley et al, 1996, p. 593.
  //
  //  H = 0.0 to 360.0 (corresponding to 0..360 degrees around hexcone)
  //      NaN (undefined) for S = 0
  //  S = 0.0 (shade of gray) to 1.0 (pure color)
  //  V = 0.0 (black)         to 1.0 (white)

  PROCEDURE  HSVtoRGB (CONST H,S,V:  TReal; VAR R,G,B:  TReal);
    VAR
      f    :  TReal;
      i    :  INTEGER;
      hTemp:  TReal;              // since H is CONST parameter
      p,q,t:  TReal;
  BEGIN
    IF   S = 0.0                  // color is on black-and-white center line
    THEN BEGIN
      IF   IsNaN(H)
      THEN BEGIN
        R := V;                   // achromatic:  shades of gray
        G := V;
        B := V
      END
      ELSE RAISE EColorError.Create('HSVtoRGB:  S = 0 and H has a value');
    END

    ELSE BEGIN                    // chromatic color
      IF   H = 360.0              // 360 degrees same as 0 degrees
      THEN hTemp := 0.0
      ELSE hTemp := H;

      hTemp := hTemp / 60;        // h is now IN [0,6)
      i := TRUNC(hTemp);          // largest integer <= h
      f := hTemp - i;             // fractional part of h

      p := V * (1.0 - S);
      q := V * (1.0 - (S * f));
      t := V * (1.0 - (S * (1.0 - f)));

      CASE i OF
        0:  BEGIN R := V;  G := t;  B := p  END;
        1:  BEGIN R := q;  G := V;  B := p  END;
        2:  BEGIN R := p;  G := V;  B := t  END;
        3:  BEGIN R := p;  G := q;  B := V  END;
        4:  BEGIN R := t;  G := p;  B := V  END;
        5:  BEGIN R := V;  G := p;  B := q  END
      END
    END
  END {HSVtoRGB};


  // RGB, each 0 to 255, to HSV.
  //   H = 0.0 to 360.0 (corresponding to 0..360.0 degrees around hexcone)
  //   S = 0.0 (shade of gray) to 1.0 (pure color)
  //   V = 0.0 (black) to 1.0 {white)

  // Based on C Code in "Computer Graphics -- Principles and Practice,"
  // Foley et al, 1996, p. 592.  Floating point fractions, 0..1, replaced with
  // integer values, 0..255.

  PROCEDURE RGBToHSV (CONST R,G,B:  TReal; VAR H,S,V:  TReal);
    VAR
      Delta:  TReal;
      Min  :  TReal;
  BEGIN
    Min := MinValue( [R, G, B] );
    V   := MaxValue( [R, G, B] );

    Delta := V - Min;

    // Calculate saturation:  saturation is 0 if r, g and b are all 0
    IF   V =  0.0
    THEN S := 0
    ELSE S := Delta / V;

    IF   S  = 0.0
    THEN H := NaN // Achromatic:  When s = 0, h is undefined
    ELSE BEGIN    // Chromatic
      IF   R = V
      THEN  // between yellow and magenta [degrees]
            H := 60.0 * (G - B) / Delta
      ELSE
        IF   G = V
        THEN // between cyan and yellow
             H := 120.0 + 60.0 * (B - R) / Delta
        ELSE
          IF  B = V
          THEN // between magenta and cyan
               H := 240.0 + 60.0 * (R - G) / Delta;

      IF   H < 0.0
      THEN H := H + 360.0
    END
  END {RGBtoHSV};


  FUNCTION CreateHueSaturationCircle(CONST size:  INTEGER;
                                     CONST HueLevel:  INTEGER;
                                     CONST SaturationLevel:  INTEGER;
                                     CONST ValueLevel:  INTEGER;
                                     CONST BackgroundColor:  TColor;
                                     CONST SaturationCircleColor:  TColor;
                                     CONST HueLineColor:  TColor):  TBitmap;
    VAR
      angle        :  Double;
      delta        :  INTEGER;
      dSquared     :  INTEGER;
      H,S,V        :  INTEGER;
      i            :  INTEGER;
      j            :  INTEGER;
      Radius       :  INTEGER;
      RadiusSquared:  INTEGER;
      row          :  pRGBTripleArray;
      X            :  INTEGER;
      Y            :  INTEGER;
  BEGIN
    RESULT := TBitmap.Create;
    RESULT.PixelFormat := pf24bit;
    RESULT.Width  := size;
    RESULT.Height := size;

    // Fill with background color
    RESULT.Canvas.Brush.Color := BackGroundColor;
    RESULT.Canvas.FillRect(RESULT.Canvas.ClipRect);

    Radius := size DIV 2;
    RadiusSquared := Radius*Radius;

    V := ValueLevel;
    FOR j := 0 TO RESULT.Height-1 DO
    BEGIN
      Y := Size - 1 - j - Radius;  {Center is Radius offset}
      row := RESULT.Scanline[Size - 1 - j];

      FOR i := 0 TO RESULT.Width-1 DO
      BEGIN
        X := i - Radius;
        dSquared := X*X + Y*Y;

        IF  dSquared <= RadiusSquared
        THEN BEGIN
          S := ROUND( (255 * SQRT(dSquared)) / Radius );
          H := ROUND( 180 * (1 + ArcTan2(X, Y) / PI));   // 0..360 degrees

          // Shift 90 degrees so H=0 (red) occurs along "X" axis
          H := H + 90;
          IF   H > 360
          THEN H := H - 360;

          row[i] := HSVtoRGBTriple(H,S,V)
        END
      END;

    END;

    // Draw Saturation Circle
    // ap
    // changed to the way Maya 2.0 doing this

    IF   (SaturationLevel <> 0) AND
         ((HueLevel >= 0) AND (HueLevel <= 360))
    THEN BEGIN
      // Use negative value for counterclockwise angles with the "Y"
      // direction going the "wrong" (mathematical) way
      Angle := -HueLevel * PI / 180;
      delta := MulDiv(Radius, SaturationLevel, 100);

      RESULT.Canvas.Brush.Style := bsClear;
{      RESULT.Canvas.MoveTo(Radius,Radius);
      RESULT.Canvas.LineTo(radius+Round(delta*COS(angle)),
                           radius+Round(delta*SIN(angle)))}
      x := radius+Round(delta*COS(angle));
      y := radius+Round(delta*SIN(angle));
      RESULT.Canvas.Pen.Color := HueLineColor;
      RESULT.Canvas.Ellipse(x - 3,
                            y - 3,
                            x + 3,
                            y + 3);
// this is for similarity with Maya only - might be removed easily
      RESULT.Canvas.Pen.Color := clWhite;
      RESULT.Canvas.Ellipse(x - 2,
                            y - 2,
                            x + 2,
                            y + 2);

    END;

    // --ap

  END {CreateHueSaturationCircle};

end.
