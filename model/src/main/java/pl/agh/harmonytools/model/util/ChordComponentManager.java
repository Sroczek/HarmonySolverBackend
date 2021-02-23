package pl.agh.harmonytools.model.util;

import pl.agh.harmonytools.model.ChordComponent;

import java.util.HashMap;

public class ChordComponentManager {

    private static final HashMap<String, ChordComponent> availableChordComponents = new HashMap<>();

    public static ChordComponent chordComponentFromString(String chordComponentString, boolean isDown) {
        if (availableChordComponents.containsKey(chordComponentString + (isDown ? "_down" : ""))) {
            System.out.println("CONTAINS KEY");
            return availableChordComponents.get(chordComponentString + (isDown ? "_down" : ""));
        } else {
            ChordComponent cc = new ChordComponent(chordComponentString, isDown);
            availableChordComponents.put(chordComponentString + (isDown ? "_down" : ""), cc);
            return cc;
        }
    }

    @Deprecated
    public static ChordComponent basicChordComponentFromPitch(Integer pitch, boolean isDown) {
        switch (pitch) {
            case 3:
                return chordComponentFromString("3>", isDown);
            case 4:
                return chordComponentFromString("3", isDown);
            case 5:
                return chordComponentFromString("3<", isDown);
            case 6:
                return chordComponentFromString("5>", isDown);
            case 7:
                return chordComponentFromString("5", isDown);
            case 8:
                return chordComponentFromString("5<", isDown);
        }
        throw new IllegalArgumentException("Unsupported pitch");
    }
}
