package pl.agh.harmonytools.model;

import lombok.Getter;

@Getter
public class MajorScale extends Scale {

    private final int[] pitches = {0, 2, 4, 5, 7, 9, 11};
    private Key key;

    public MajorScale(Key key) {
        super(key);
    }

    public MajorScale(BaseNote basenote, int tonicPitch){
        super(basenote, tonicPitch);
    }
}

