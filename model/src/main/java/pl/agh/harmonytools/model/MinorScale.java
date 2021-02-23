package pl.agh.harmonytools.model;

import lombok.Getter;

@Getter
public class MinorScale extends Scale {

    private final int[] pitches = {0, 2, 3, 5, 7, 8, 10};
    private Key key;

    public MinorScale(Key key) {
        super(key);
    }

    public MinorScale(BaseNote basenote, int tonicPitch){
        super(basenote, tonicPitch);
    }
}

