package pl.agh.harmonytools.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

import java.util.List;

@Getter
@Setter
@Builder
@AllArgsConstructor
public class HarmonicFunction {

    private String name;
    private int degree;
    private ChordComponent position;
    private ChordComponent revolution;
    private Delay delay;
    private List<ChordComponent> extra;
    private List<ChordComponent> omit;
    private Boolean isDown;
    private ChordSystem system;
    private Mode mode;
    private Key key;

}
