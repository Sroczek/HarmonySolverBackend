package pl.agh.harmonytools.model;

import lombok.Getter;

import java.util.HashMap;

@Getter
public class ChordComponent {

    static final HashMap<Integer, Integer> baseComponentPitch = new HashMap<>();
    static {
        baseComponentPitch.put(1, 0);
        baseComponentPitch.put(2, 2);
        baseComponentPitch.put(3, 4);
        baseComponentPitch.put(4, 5);
        baseComponentPitch.put(5, 7);
        baseComponentPitch.put(6, 9);
        baseComponentPitch.put(7, 10);
        baseComponentPitch.put(8, 12);
        baseComponentPitch.put(9, 14);
    }

    private String chordComponentString;
    private Integer baseComponent;
    private long semitonesNumber;
    private boolean isDown;

    public ChordComponent(String chordComponentString, boolean isDown){
        this.chordComponentString = chordComponentString;
        this.baseComponent = Integer.parseInt(chordComponentString.substring(0,1));
        this.semitonesNumber = baseComponentPitch.get(baseComponent)
                + chordComponentString.codePoints().filter(ch -> ch == '<').count()
                - chordComponentString.codePoints().filter(ch -> ch == '>').count()
                + (isDown ? -1 : 0);
        this.isDown = isDown;
    }

    // should be moved from here
    public String toXmlString() {
        return this.chordComponentString.replace("<", "&lt;").replace(">", "&gt;");
    }
}
