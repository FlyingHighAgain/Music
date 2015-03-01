module Music.Theory(
	Music.Theory.Internal.Notes.Note(..),
	Music.Theory.Internal.Scales.Scale(..),
	Music.Theory.Internal.Key.Key(..),
	Music.Theory.Internal.Key.toKey,
    Music.Theory.Internal.ScaleNotes.getScaleNotes,
    Music.Theory.Internal.ChordNotes.getChordNotes,
    Music.Theory.Internal.RomanNumeral.toRomanNumeral,
    Music.Theory.Internal.RomanNumeral.fromRomanNumeral,
    Music.Theory.Internal.ChordFunction.toChordFunction
    ) where

import Music.Theory.Internal.Notes
import Music.Theory.Internal.Scales
import Music.Theory.Internal.Key
import Music.Theory.Internal.ScaleNotes
import Music.Theory.Internal.ChordNotes
import Music.Theory.Internal.RomanNumeral
import Music.Theory.Internal.ChordFunction

