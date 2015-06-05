# Introduction #

This package was designed as a place where TUO stores "all" the different string similarity routines around. To see how these routines work internally you are invited to read the code ;-) or look up at wikipedia :-D

# Details #

By now there are two different methods of string similarity routines in the package. Each unit contains one or the other ;-) simples

## Phonetic comparison ##

These routines compare the string similarity by the "sound" of the word.

  * uKoelnerPhonetik.pas
  * uSoundEx.pas
  * uMetaphone.pas
  * DoubleMetaphone.pas

As there is no way of telling that a word sounds 20 or 80 per cent alike the function returns only a boolean value:

```
function SoundsSimilar(const AStr1, AStr2: String): Boolean;
```

## Similarity ratio ##

The other way of string comparison is to calculate how similar two strings are in per cent.

uNeedlemanWunch in '..\..\uNeedlemanWunch.pas',
  * uDiceSimilarity.pas
  * FuzzyMatching.pas ... by NGram
  * uJaroWinkler.pas
  * uKoelnerPhonetik.pas
  * uLevenshtein.pas
  * uSmithWatermanGotoh.pas
  * uMongeElkan.pas

The appropriate routine returns a double value between 0 and 1:

```
function StringSimilarityRatio(const Str1, Str2: String; IgnoreCase: Boolean): Double;
```