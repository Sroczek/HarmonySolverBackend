package pl.agh.harmonytools.model;

import lombok.Getter;

import java.util.*;
import java.util.stream.Collectors;

@Getter
public class Key {

    private static final HashMap<String, Integer> keyStrPitch = new HashMap<>();
    private static final HashMap<Integer, Set<String>> pitchKeyStr = new HashMap<>();

    static {
        keyStrPitch.put("C", 60);
        keyStrPitch.put("B#", 60);
        keyStrPitch.put("Dbb", 60);
        keyStrPitch.put("C#", 61);
        keyStrPitch.put("Db", 61);
        keyStrPitch.put("B##", 61);
        keyStrPitch.put("D", 62);
        keyStrPitch.put("C##", 62);
        keyStrPitch.put("Ebb", 62);
        keyStrPitch.put("Eb", 63);
        keyStrPitch.put("D#", 63);
        keyStrPitch.put("Fbb", 63);
        keyStrPitch.put("E", 64);
        keyStrPitch.put("D##", 64);
        keyStrPitch.put("Fb", 64);
        keyStrPitch.put("F", 65);
        keyStrPitch.put("E#", 65);
        keyStrPitch.put("Gbb", 65);
        keyStrPitch.put("F#", 66);
        keyStrPitch.put("Gb", 66);
        keyStrPitch.put("E##", 66);
        keyStrPitch.put("G", 67);
        keyStrPitch.put("F##", 67);
        keyStrPitch.put("Abb", 67);
        keyStrPitch.put("Ab", 68);
        keyStrPitch.put("G#", 68);
        keyStrPitch.put("A", 69);
        keyStrPitch.put("G##", 69);
        keyStrPitch.put("Bbb", 69);
        keyStrPitch.put("Bb", 70);
        keyStrPitch.put("A#", 70);
        keyStrPitch.put("Cbb", 70);
        keyStrPitch.put("B", 71);
        keyStrPitch.put("Cb", 71);
        keyStrPitch.put("A##", 71);
        keyStrPitch.put("c", 60);
        keyStrPitch.put("b#", 60);
        keyStrPitch.put("dbb", 60);
        keyStrPitch.put("c#", 61);
        keyStrPitch.put("db", 61);
        keyStrPitch.put("b##", 61);
        keyStrPitch.put("d", 62);
        keyStrPitch.put("c##", 62);
        keyStrPitch.put("ebb", 62);
        keyStrPitch.put("d#", 63);
        keyStrPitch.put("eb", 63);
        keyStrPitch.put("fbb", 63);
        keyStrPitch.put("e", 64);
        keyStrPitch.put("d##", 64);
        keyStrPitch.put("fb", 64);
        keyStrPitch.put("f", 65);
        keyStrPitch.put("e#", 65);
        keyStrPitch.put("gbb", 65);
        keyStrPitch.put("f#", 66);
        keyStrPitch.put("gb", 66);
        keyStrPitch.put("e##", 66);
        keyStrPitch.put("g", 67);
        keyStrPitch.put("f##", 67);
        keyStrPitch.put("abb", 67);
        keyStrPitch.put("g#", 68);
        keyStrPitch.put("ab", 68);
        keyStrPitch.put("a", 69);
        keyStrPitch.put("g##", 69);
        keyStrPitch.put("bbb", 69);
        keyStrPitch.put("a#", 70);
        keyStrPitch.put("bb", 70);
        keyStrPitch.put("cbb", 70);
        keyStrPitch.put("b", 71);
        keyStrPitch.put("cb", 71);
        keyStrPitch.put("a##", 71);

        keyStrPitch.forEach((key, value) -> {
            if (pitchKeyStr.containsKey(value)) {
                pitchKeyStr.get(value).add(key);
            } else {
                Set<String> set = new HashSet<>();
                set.add(key);
                pitchKeyStr.put(value, set);
            }
        });

    }

    private final String keySignature;
    private final Integer tonicPitch;
    private final BaseNote baseNote;

    public Key(String keySignature) {
        this.baseNote = baseNoteFromKeySignature(keySignature);
        this.keySignature = keySignature;
        this.tonicPitch = keyStrPitch.get(keySignature);
    }

    public Key(BaseNote baseNote, Integer tonicPitch) {
        this.baseNote = baseNote;
        this.tonicPitch = tonicPitch;
        this.keySignature = inferKeySingature(baseNote, tonicPitch);
    }

    private BaseNote baseNoteFromKeySignature(String keySignature) {
        try {
            return BaseNote.valueOf(keySignature.toUpperCase().substring(0, 1));
        } catch (IllegalArgumentException e) {
            throw new IllegalArgumentException("Unsupported key signature", e);
        }
    }

    private String inferKeySingature(BaseNote baseNote, Integer tonicPitch) {

        List<String> possibleKeySignatures = pitchKeyStr.get(tonicPitch)
                .stream()
                .filter(keyCandidate -> baseNoteFromKeySignature(keyCandidate).equals(this.baseNote))
                .collect(Collectors.toList());

        switch (possibleKeySignatures.size()) {
            case 0:
                throw new IllegalArgumentException("Cannot infer a key");
            case 1:
                return possibleKeySignatures.get(0);
            default:
                throw new IllegalArgumentException("Cannot infer a key - to many possible keys for given arguments");
        }
    }
}
