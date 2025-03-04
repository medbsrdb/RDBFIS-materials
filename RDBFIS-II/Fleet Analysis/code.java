Fleet analysis source code 
src/main/java/eu/medbsrdb/rdbfisapi/fleetanalysismodel/FleetAnalysisInput.java

package eu.medbsrdb.rdbfisapi.fleetanalysismodel;


import java.math.BigDecimal;

public class FleetAnalysisInput {

    private Short year;
    private String event;
    private Short durationStart;
    private Short durationEnd;
    private String vesselLengthSegmentationType;
    private Long numOfVessels;
    private Long numOfVesselsBetween;
    private Integer operatorNumOfVessels;
    private BigDecimal gt;
    private BigDecimal gtBetween;
    private Integer operatorGt;
    private BigDecimal kw;
    private BigDecimal kwBetween;
    private Integer operatorKw;
    private Short age;
    private Short ageBetween;
    private Integer operatorAge;
    private BigDecimal length;
    private BigDecimal lengthBetween;
    private Integer operatorLength;
    private String[] countries;
    private String[] portNames;
    private String[] nuts2Regions;
    private String[] nuts3Prefectures;
    private String[] fdiSubregions;
    private String[] fishingAreas;
    private String[] geoAreas;
    private String[] gears;
    private boolean groupByCountry;
    private boolean groupByPort;
    private boolean groupByNuts2Region;
    private boolean groupByNuts3Prefecture;
    private boolean groupByFdiSubregion;
    private boolean groupByFishingArea;
    private boolean groupByGeoArea;
    private boolean groupByGearType;
    private boolean groupByLengthClass;
    private boolean groupByAgeClass;

    public Short getYear() {
        return year;
    }

    public void setYear(Short year) {
        this.year = year;
    }

    public String getEvent() {
        return event;
    }

    public void setEvent(String event) {
        this.event = event;
    }

    public Short getDurationStart() {
        return durationStart;
    }

    public void setDurationStart(Short durationStart) {
        this.durationStart = durationStart;
    }

    public Short getDurationEnd() {
        return durationEnd;
    }

    public void setDurationEnd(Short durationEnd) {
        this.durationEnd = durationEnd;
    }

    public String getVesselLengthSegmentationType() {
        return vesselLengthSegmentationType;
    }

    public void setVesselLengthSegmentationType(String vesselLengthSegmentationType) {
        this.vesselLengthSegmentationType = vesselLengthSegmentationType;
    }

    public Long getNumOfVessels() {
        return numOfVessels;
    }

    public void setNumOfVessels(Long numOfVessels) {
        this.numOfVessels = numOfVessels;
    }

    public Long getNumOfVesselsBetween() {
        return numOfVesselsBetween;
    }

    public void setNumOfVesselsBetween(Long numOfVesselsBetween) {
        this.numOfVesselsBetween = numOfVesselsBetween;
    }

    public Integer getOperatorNumOfVessels() {
        return operatorNumOfVessels;
    }

    public void setOperatorNumOfVessels(Integer operatorNumOfVessels) {
        this.operatorNumOfVessels = operatorNumOfVessels;
    }

    public BigDecimal getGt() {
        return gt;
    }

    public void setGt(BigDecimal gt) {
        this.gt = gt;
    }

    public BigDecimal getGtBetween() {
        return gtBetween;
    }

    public void setGtBetween(BigDecimal gtBetween) {
        this.gtBetween = gtBetween;
    }

    public Integer getOperatorGt() {
        return operatorGt;
    }

    public void setOperatorGt(Integer operatorGt) {
        this.operatorGt = operatorGt;
    }

    public BigDecimal getKw() {
        return kw;
    }

    public void setKw(BigDecimal kw) {
        this.kw = kw;
    }

    public BigDecimal getKwBetween() {
        return kwBetween;
    }

    public void setKwBetween(BigDecimal kwBetween) {
        this.kwBetween = kwBetween;
    }

    public Integer getOperatorKw() {
        return operatorKw;
    }

    public void setOperatorKw(Integer operatorKw) {
        this.operatorKw = operatorKw;
    }

    public Short getAge() {
        return age;
    }

    public void setAge(Short age) {
        this.age = age;
    }

    public Short getAgeBetween() {
        return ageBetween;
    }

    public void setAgeBetween(Short ageBetween) {
        this.ageBetween = ageBetween;
    }

    public Integer getOperatorAge() {
        return operatorAge;
    }

    public void setOperatorAge(Integer operatorAge) {
        this.operatorAge = operatorAge;
    }

    public BigDecimal getLength() {
        return length;
    }

    public void setLength(BigDecimal length) {
        this.length = length;
    }

    public BigDecimal getLengthBetween() {
        return lengthBetween;
    }

    public void setLengthBetween(BigDecimal lengthBetween) {
        this.lengthBetween = lengthBetween;
    }

    public Integer getOperatorLength() {
        return operatorLength;
    }

    public void setOperatorLength(Integer operatorLength) {
        this.operatorLength = operatorLength;
    }

    public String[] getCountries() {
        return countries;
    }

    public void setCountries(String[] countries) {
        this.countries = countries;
    }

    public String[] getPortNames() {
        return portNames;
    }

    public void setPortNames(String[] portNames) {
        this.portNames = portNames;
    }

    public String[] getNuts2Regions() {
        return nuts2Regions;
    }

    public void setNuts2Regions(String[] nuts2Regions) {
        this.nuts2Regions = nuts2Regions;
    }

    public String[] getNuts3Prefectures() {
        return nuts3Prefectures;
    }

    public void setNuts3Prefectures(String[] nuts3prefectures) {
        this.nuts3Prefectures = nuts3prefectures;
    }

    public String[] getFdiSubregions() {
        return fdiSubregions;
    }

    public void setFdiSubregions(String[] fdiSubregions) {
        this.fdiSubregions = fdiSubregions;
    }

    public String[] getFishingAreas() {
        return fishingAreas;
    }

    public void setFishingAreas(String[] fishingAreas) {
        this.fishingAreas = fishingAreas;
    }

    public String[] getGeoAreas() {
        return geoAreas;
    }

    public void setGeoAreas(String[] geoAreas) {
        this.geoAreas = geoAreas;
    }

    public boolean isGroupByCountry() {
        return groupByCountry;
    }

    public void setGroupByCountry(boolean groupByCountry) {
        this.groupByCountry = groupByCountry;
    }

    public boolean isGroupByPort() {
        return groupByPort;
    }

    public void setGroupByPort(boolean groupByPort) {
        this.groupByPort = groupByPort;
    }

    public boolean isGroupByNuts2Region() {
        return groupByNuts2Region;
    }

    public void setGroupByNuts2Region(boolean groupByNuts2Region) {
        this.groupByNuts2Region = groupByNuts2Region;
    }

    public boolean isGroupByNuts3Prefecture() {
        return groupByNuts3Prefecture;
    }

    public void setGroupByNuts3Prefecture(boolean groupByNuts3Prefecture) {
        this.groupByNuts3Prefecture = groupByNuts3Prefecture;
    }

    public boolean isGroupByFdiSubregion() {
        return groupByFdiSubregion;
    }

    public void setGroupByFdiSubregion(boolean groupByFdiSubregion) {
        this.groupByFdiSubregion = groupByFdiSubregion;
    }

    public boolean isGroupByFishingArea() {
        return groupByFishingArea;
    }

    public void setGroupByFishingArea(boolean groupByFishingArea) {
        this.groupByFishingArea = groupByFishingArea;
    }

    public boolean isGroupByGeoArea() {
        return groupByGeoArea;
    }

    public void setGroupByGeoArea(boolean groupByGeoArea) {
        this.groupByGeoArea = groupByGeoArea;
    }

    public boolean isGroupByGearType() {
        return groupByGearType;
    }

    public void setGroupByGearType(boolean groupByGearType) {
        this.groupByGearType = groupByGearType;
    }

    public boolean isGroupByLengthClass() {
        return groupByLengthClass;
    }

    public void setGroupByLengthClass(boolean groupByLengthClass) {
        this.groupByLengthClass = groupByLengthClass;
    }

    public boolean isGroupByAgeClass() {
        return groupByAgeClass;
    }

    public void setGroupByAgeClass(boolean groupByAgeClass) {
        this.groupByAgeClass = groupByAgeClass;
    }

    public String[] getGears() {
        return gears;
    }

    public void setGears(String[] gears) {
        this.gears = gears;
    }
}

src/main/java/eu/medbsrdb/rdbfisapi/fleetanalysismodel/FleetAnalysisResult.java
package eu.medbsrdb.rdbfisapi.fleetanalysismodel;

public class FleetAnalysisResult {

    private Short year;
    private String country;
    private Long numberOfVessels;
    private String event;
    private String gearType;
    private Double sumGT;
    private Double sumKW;
    private Double averageVesselAge;
    private Double averageVesselLength;
    private String portName;
    private String nuts2Region;
    private String nuts3Prefecture;
    private String fdiSubregion;
    private String fishingArea;
    private String geoArea;
    private String ageClass;
    private String lengthClass;

    public Short getYear() {
        return year;
    }

    public void setYear(Short year) {
        this.year = year;
    }

    public String getCountry() {
        return country;
    }

    public void setCountry(String country) {
        this.country = country;
    }

    public Long getNumberOfVessels() {
        return numberOfVessels;
    }

    public void setNumberOfVessels(Long numberOfVessels) {
        this.numberOfVessels = numberOfVessels;
    }

    public String getEvent() {
        return event;
    }

    public void setEvent(String event) {
        this.event = event;
    }

    public String getGearType() {
        return gearType;
    }

    public void setGearType(String gearType) {
        this.gearType = gearType;
    }

    public Double getSumGT() {
        return sumGT;
    }

    public void setSumGT(Double sumGT) {
        this.sumGT = sumGT;
    }

    public Double getSumKW() {
        return sumKW;
    }

    public void setSumKW(Double sumKW) {
        this.sumKW = sumKW;
    }

    public Double getAverageVesselAge() {
        return averageVesselAge;
    }

    public void setAverageVesselAge(Double averageVesselAge) {
        this.averageVesselAge = averageVesselAge;
    }

    public Double getAverageVesselLength() {
        return averageVesselLength;
    }

    public void setAverageVesselLength(Double averageVesselLength) {
        this.averageVesselLength = averageVesselLength;
    }

    public String getPortName() {
        return portName;
    }

    public void setPortName(String portName) {
        this.portName = portName;
    }

    public String getNuts2Region() {
        return nuts2Region;
    }

    public void setNuts2Region(String nuts2Region) {
        this.nuts2Region = nuts2Region;
    }

    public String getNuts3Prefecture() {
        return nuts3Prefecture;
    }

    public void setNuts3Prefecture(String nuts3Prefecture) {
        this.nuts3Prefecture = nuts3Prefecture;
    }

    public String getFdiSubregion() {
        return fdiSubregion;
    }

    public void setFdiSubregion(String fdiSubregion) {
        this.fdiSubregion = fdiSubregion;
    }

    public String getFishingArea() {
        return fishingArea;
    }

    public void setFishingArea(String fishingArea) {
        this.fishingArea = fishingArea;
    }

    public String getGeoArea() {
        return geoArea;
    }

    public void setGeoArea(String geoArea) {
        this.geoArea = geoArea;
    }

    public String getAgeClass() {
        return ageClass;
    }

    public void setAgeClass(String ageClass) {
        this.ageClass = ageClass;
    }

    public String getLengthClass() {
        return lengthClass;
    }

    public void setLengthClass(String lengthClass) {
        this.lengthClass = lengthClass;
    }
}

src/main/java/eu/medbsrdb/rdbfisapi/services/FleetAnalysisActiveService.java
package eu.medbsrdb.rdbfisapi.services;

import com.querydsl.core.BooleanBuilder;
import com.querydsl.core.types.Expression;
import com.querydsl.core.types.Projections;
import com.querydsl.core.types.dsl.Expressions;
import com.querydsl.core.types.dsl.NumberExpression;
import com.querydsl.core.types.dsl.NumberTemplate;
import com.querydsl.core.types.dsl.StringPath;
import com.querydsl.jpa.impl.JPAQuery;
import com.querydsl.jpa.impl.JPAQueryFactory;
import eu.medbsrdb.rdbfisapi.fleetanalysismodel.FleetAnalysisInput;
import eu.medbsrdb.rdbfisapi.fleetanalysismodel.FleetAnalysisResult;
import eu.medbsrdb.rdbfisapi.entities.QFleetEuActive;
import eu.medbsrdb.rdbfisapi.entities.QPPortsEuFleetRegCircabcNut;
import org.springframework.stereotype.Service;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.transaction.Transactional;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import static com.querydsl.core.types.dsl.Expressions.numberTemplate;

@Service
public class FleetAnalysisActiveService {

    public static final int EQUALS = 1;
    public static final int GREATER_THAN = 2;
    public static final int LESS_THAN = 3;
    public static final int BETWEEN = 4;

    @PersistenceContext
    private EntityManager entityManager;

    @Transactional
    public List<FleetAnalysisResult> fleetAnalysis(FleetAnalysisInput input) {
        QFleetEuActive qFleet = QFleetEuActive.fleetEuActive;
        QPPortsEuFleetRegCircabcNut qpPorts = QPPortsEuFleetRegCircabcNut.pPortsEuFleetRegCircabcNut;

        JPAQueryFactory qf = new JPAQueryFactory(entityManager);

        List<Expression> columns = constructProjections(input.getVesselLengthSegmentationType(), input.isGroupByCountry(), input.isGroupByPort(), input.isGroupByNuts2Region(),
                input.isGroupByNuts3Prefecture(), input.isGroupByFdiSubregion(), input.isGroupByFishingArea(), input.isGroupByGeoArea(), input.isGroupByGearType(), input.isGroupByLengthClass(),
                input.isGroupByAgeClass());

        BooleanBuilder whereClause = constructFilters(input.getYear(), input.getDurationStart(), input.getDurationEnd(), input.getNumOfVessels(), input.getNumOfVesselsBetween(),
                input.getOperatorNumOfVessels(), input.getGt(), input.getGtBetween(), input.getOperatorGt(), input.getKw(), input.getKwBetween(), input.getOperatorKw(),
                input.getAge(), input.getAgeBetween(), input.getOperatorAge(), input.getLength(), input.getLengthBetween(), input.getOperatorLength(), input.getCountries(),
                input.getPortNames(), input.getNuts2Regions(), input.getNuts3Prefectures(), input.getFdiSubregions(), input.getFishingAreas(), input.getGeoAreas(), input.getGears());

        JPAQuery<FleetAnalysisResult> query = qf.select(Projections.bean(FleetAnalysisResult.class, columns.toArray(new Expression<?>[0]))).from(qFleet);

        query = query.where(whereClause);

        if (input.isGroupByPort() || input.isGroupByNuts2Region() || input.isGroupByNuts3Prefecture() || input.isGroupByFdiSubregion() || input.isGroupByFishingArea()
                || input.isGroupByGeoArea() || input.getPortNames().length > 0 || input.getNuts2Regions().length > 0 || input.getNuts3Prefectures().length > 0
                || input.getFdiSubregions().length > 0 || input.getFishingAreas().length > 0 || input.getGeoAreas().length > 0)
            query = query.innerJoin(qpPorts).on(qFleet.placeOfRegistration.eq(qpPorts.portCodeFr).and(qFleet.country.eq(qpPorts.country)));

        query = query.groupBy(constructGroupBy(input).toArray(new Expression<?>[0]));

        // Define the composite expression for filtering as well:
        NumberExpression<Long> distinctVesselCount = Expressions.numberTemplate(
                Long.class,
                "count(DISTINCT CONCAT({0}, '-', {1}, '-', {2}))",
                qFleet.country,
                qFleet.year.stringValue(),
                qFleet.cfr
        );

        // Then add a HAVING clause based on the operator:
        if (input.getOperatorNumOfVessels() != null && input.getNumOfVessels() != null) {
            switch(input.getOperatorNumOfVessels()) {
                case EQUALS:
                    query.having(distinctVesselCount.eq(input.getNumOfVessels()));
                    break;
                case GREATER_THAN:
                    query.having(distinctVesselCount.gt(input.getNumOfVessels()));
                    break;
                case LESS_THAN:
                    query.having(distinctVesselCount.lt(input.getNumOfVessels()));
                    break;
                case BETWEEN:
                    query.having(distinctVesselCount.between(input.getNumOfVessels(), input.getNumOfVesselsBetween()));
                    break;
            }
        }

        return query.fetch();
    }

    private List<Expression> constructGroupBy(FleetAnalysisInput input) {
        List<Expression> expressions = new ArrayList<>();
        QFleetEuActive qFleet = QFleetEuActive.fleetEuActive;
        QPPortsEuFleetRegCircabcNut qpPorts = QPPortsEuFleetRegCircabcNut.pPortsEuFleetRegCircabcNut;
        expressions.add(qFleet.year);
        if (input.isGroupByCountry())
            expressions.add(qFleet.country);
        if (input.isGroupByPort()) {
            expressions.add(qpPorts.portName);
            if (!expressions.contains(qFleet.country))
                expressions.add(qFleet.country);
        }
        if (input.isGroupByNuts2Region())
            expressions.add(qpPorts.nuts2Region);
        if (input.isGroupByNuts3Prefecture())
            expressions.add(qpPorts.nuts3Prefecture);
        if (input.isGroupByFdiSubregion())
            expressions.add(qpPorts.fdiSubregion);
        if (input.isGroupByFishingArea())
            expressions.add(qpPorts.fishingArea);
        if (input.isGroupByGeoArea())
            expressions.add(qpPorts.geoArea);
        if (input.isGroupByGearType())
            expressions.add(qFleet.gearType);
        if (input.isGroupByAgeClass())
            expressions.add(qFleet.ageClass);
        if (input.isGroupByLengthClass()) {
            if (input.getVesselLengthSegmentationType().equals("medbs"))
                expressions.add(qFleet.vlMedbs);
            else if (input.getVesselLengthSegmentationType().equals("otherwaters"))
                expressions.add(qFleet.vlOtherWaters);
            else if (input.getVesselLengthSegmentationType().equals("balticsea"))
                expressions.add(qFleet.vlBalticSea);
            else if (input.getVesselLengthSegmentationType().equals("gfcm"))
                expressions.add(qFleet.vlGfcm);
        }
        return expressions;
    }

    private List<Expression> constructProjections(String vesselLengthSegmentationType, boolean groupByCountry, boolean groupByPort, boolean groupByNuts2Region, boolean groupByNuts3Prefecture,
                                                                           boolean groupByFdiSubregion, boolean groupByFishingArea, boolean groupByGeoArea, boolean groupByGearType,
                                                                           boolean groupByLengthClass, boolean groupByAgeClass) {
        List<Expression> expressions = new ArrayList<>();
        QFleetEuActive qFleet = QFleetEuActive.fleetEuActive;
        QPPortsEuFleetRegCircabcNut qpPorts = QPPortsEuFleetRegCircabcNut.pPortsEuFleetRegCircabcNut;
        StringPath vesselLength = null;
        if (vesselLengthSegmentationType.equals("medbs"))
            vesselLength = qFleet.vlMedbs;
        else if (vesselLengthSegmentationType.equals("otherwaters"))
            vesselLength = qFleet.vlOtherWaters;
        else if (vesselLengthSegmentationType.equals("balticsea"))
            vesselLength = qFleet.vlBalticSea;
        else if (vesselLengthSegmentationType.equals("gfcm"))
            vesselLength = qFleet.vlGfcm;
        if (groupByLengthClass)
            expressions.add(vesselLength.as("lengthClass"));
        if (groupByNuts2Region)
            expressions.add(qpPorts.nuts2Region);
        if (groupByNuts3Prefecture)
            expressions.add(qpPorts.nuts3Prefecture);
        if (groupByFdiSubregion)
            expressions.add(qpPorts.fdiSubregion);
        if (groupByFishingArea)
            expressions.add(qpPorts.fishingArea);
        if (groupByGeoArea)
            expressions.add(qpPorts.geoArea);
        if (groupByGearType)
            expressions.add(qFleet.gearType);
        if (groupByAgeClass)
            expressions.add(qFleet.ageClass);
        if (groupByCountry)
            expressions.add(qFleet.country);
        if (groupByPort) {
            expressions.add(qpPorts.portName);
            if (!expressions.contains(qFleet.country))
                expressions.add(qFleet.country);
        }
        expressions.add(qFleet.year);
        expressions.add(qFleet.gt.doubleValue().sum().round().as("sumGT"));
        expressions.add(qFleet.kw.doubleValue().sum().round().as("sumKW"));
        NumberExpression avLength = roundToTwoDecimalPlaces(qFleet.loa.doubleValue().avg());
        expressions.add(avLength.as("averageVesselLength"));
        expressions.add(qFleet.vesselAge.doubleValue().avg().round().as("averageVesselAge"));
        NumberExpression<Long> distinctVesselCount = Expressions.numberTemplate(
                Long.class,
                "count(DISTINCT CONCAT({0}, '-', {1}, '-', {2}))",
                qFleet.country,
                qFleet.year.stringValue(),
                qFleet.cfr
        );
        expressions.add(distinctVesselCount.as("numberOfVessels"));
        return expressions;
    }

    private NumberTemplate roundToTwoDecimalPlaces(NumberExpression expression) {
        return numberTemplate(Double.class, "ROUND({0} * 100.0) / 100.0", expression);
    }

    private BooleanBuilder constructFilters(Short year, Short durationStart, Short durationEnd,
                                            Long numOfVessels, Long numOfVesselsBetween, Integer operatorNumOfVessels, BigDecimal gt,
                                            BigDecimal gtBetween, Integer operatorGt, BigDecimal kw, BigDecimal kwBetween, Integer operatorKw,
                                            Short age, Short ageBetween, Integer operatorAge, BigDecimal length, BigDecimal lengthBetween,
                                            Integer operatorLength, String[] countries, String[] portNames, String[] nuts2Regions, String[] nuts3prefectures,
                                            String[] fdiSubregions, String[] fishingAreas, String[] geoAreas, String[] gears)
    {
        BooleanBuilder whereClause = new BooleanBuilder();
        QFleetEuActive qFleet = QFleetEuActive.fleetEuActive;
        QPPortsEuFleetRegCircabcNut qpPorts = QPPortsEuFleetRegCircabcNut.pPortsEuFleetRegCircabcNut;

        if (year != null)
            whereClause.and(qFleet.year.eq(year));
        else if (durationStart != null && durationEnd != null)
            whereClause.and(qFleet.year.goe(durationStart)).and(qFleet.year.loe(durationEnd));

        /*if (operatorNumOfVessels == EQUALS)
            whereClause.and(qFleet.cfr.count().eq(numOfVessels));
        else if (operatorNumOfVessels == GREATER_THAN)
            whereClause.and(qFleet.cfr.count().gt(numOfVessels));
        else if (operatorNumOfVessels == LESS_THAN)
            whereClause.and(qFleet.cfr.count().lt(numOfVessels));
        else if (operatorNumOfVessels == BETWEEN)
            whereClause.and(qFleet.cfr.count().between(numOfVessels, numOfVesselsBetween));*/

        if (operatorGt == EQUALS)
            whereClause.and(qFleet.gt.eq(gt));
        else if (operatorGt == GREATER_THAN)
            whereClause.and(qFleet.gt.gt(gt));
        else if (operatorGt == LESS_THAN)
            whereClause.and(qFleet.gt.lt(gt));
        else if (operatorGt == BETWEEN)
            whereClause.and(qFleet.gt.between(gt,gtBetween));

        if (operatorKw == EQUALS)
            whereClause.and(qFleet.kw.eq(kw));
        else if (operatorKw == GREATER_THAN)
            whereClause.and(qFleet.kw.gt(kw));
        else if (operatorKw == LESS_THAN)
            whereClause.and(qFleet.kw.lt(kw));
        else if (operatorKw == BETWEEN)
            whereClause.and(qFleet.kw.between(kw,kwBetween));

        if (operatorAge == EQUALS)
            whereClause.and(qFleet.vesselAge.eq(age.shortValue()));
        else if (operatorAge == GREATER_THAN)
            whereClause.and(qFleet.vesselAge.gt(age.shortValue()));
        else if (operatorAge == LESS_THAN)
            whereClause.and(qFleet.vesselAge.lt(age.shortValue()));
        else if (operatorAge == BETWEEN)
            whereClause.and(qFleet.vesselAge.between(age.shortValue(),ageBetween.shortValue()));

        if (operatorLength == EQUALS)
            whereClause.and(qFleet.loa.eq(length));
        else if (operatorLength == GREATER_THAN)
            whereClause.and(qFleet.loa.gt(length));
        else if (operatorLength == LESS_THAN)
            whereClause.and(qFleet.loa.lt(length));
        else if (operatorLength == BETWEEN)
            whereClause.and(qFleet.loa.between(length, lengthBetween));

        if (countries != null && countries.length > 0 && !countries[0].isBlank()){
            BooleanBuilder countryClause = new BooleanBuilder();
            for (String country: countries) {
                countryClause.or(qFleet.country.eq(country));
            }
            whereClause.and(countryClause);
        }

        if (portNames != null && portNames.length > 0){
            BooleanBuilder portClause = new BooleanBuilder();
            for (String port: portNames) {
                portClause.or(qpPorts.portName.eq(port));
            }
            whereClause.and(portClause);
        }

        if (nuts2Regions != null && nuts2Regions.length > 0){
            BooleanBuilder nuts2Clause = new BooleanBuilder();
            for (String region: nuts2Regions) {
                nuts2Clause.or(qpPorts.nuts2Region.eq(region));
            }
            whereClause.and(nuts2Clause);
        }

        if (nuts3prefectures!= null && nuts3prefectures.length > 0){
            BooleanBuilder nuts3Clause = new BooleanBuilder();
            for (String prefecture: nuts3prefectures) {
                nuts3Clause.or(qpPorts.nuts3Prefecture.eq(prefecture));
            }
            whereClause.and(nuts3Clause);
        }

        if (fdiSubregions != null && fdiSubregions.length > 0){
            BooleanBuilder fdiClause = new BooleanBuilder();
            for (String region: fdiSubregions) {
                fdiClause.or(qpPorts.fdiSubregion.eq(region));
            }
            whereClause.and(fdiClause);
        }

        if (fishingAreas != null && fishingAreas.length > 0){
            BooleanBuilder fishingClause = new BooleanBuilder();
            for (String area: fishingAreas) {
                fishingClause.or(qpPorts.fishingArea.eq(area));
            }
            whereClause.and(fishingClause);
        }

        if (geoAreas != null && geoAreas.length > 0){
            BooleanBuilder geoClause = new BooleanBuilder();
            for (String area: geoAreas) {
                geoClause.or(qpPorts.geoArea.eq(area));
            }
            whereClause.and(geoClause);
        }

        if (gears != null && gears.length > 0) {
            BooleanBuilder gearClause = new BooleanBuilder();
            for (String gear: gears) {
                gearClause.or(qFleet.gearType.eq(gear));
            }
            whereClause.and(gearClause);
        }

        return whereClause;
    }

}

src/main/java/eu/medbsrdb/rdbfisapi/services/FleetAnalysisDecommissionedService.java
package eu.medbsrdb.rdbfisapi.services;

import com.querydsl.core.BooleanBuilder;
import com.querydsl.core.types.Expression;
import com.querydsl.core.types.Projections;
import com.querydsl.core.types.dsl.Expressions;
import com.querydsl.core.types.dsl.NumberExpression;
import com.querydsl.core.types.dsl.NumberTemplate;
import com.querydsl.core.types.dsl.StringPath;
import com.querydsl.jpa.impl.JPAQuery;
import com.querydsl.jpa.impl.JPAQueryFactory;
import eu.medbsrdb.rdbfisapi.entities.QFleetEuDecommissioned;
import eu.medbsrdb.rdbfisapi.fleetanalysismodel.FleetAnalysisInput;
import eu.medbsrdb.rdbfisapi.fleetanalysismodel.FleetAnalysisResult;
import eu.medbsrdb.rdbfisapi.entities.QFleetEuDecommissioned;
import eu.medbsrdb.rdbfisapi.entities.QPPortsEuFleetRegCircabcNut;
import org.springframework.stereotype.Service;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.transaction.Transactional;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import static com.querydsl.core.types.dsl.Expressions.numberTemplate;

@Service
public class FleetAnalysisDecommissionedService {
    public static final int EQUALS = 1;
    public static final int GREATER_THAN = 2;
    public static final int LESS_THAN = 3;
    public static final int BETWEEN = 4;

    @PersistenceContext
    private EntityManager entityManager;

    @Transactional
    public List<FleetAnalysisResult> fleetAnalysis(FleetAnalysisInput input) {
        QFleetEuDecommissioned qFleet = QFleetEuDecommissioned.fleetEuDecommissioned;
        QPPortsEuFleetRegCircabcNut qpPorts = QPPortsEuFleetRegCircabcNut.pPortsEuFleetRegCircabcNut;

        JPAQueryFactory qf = new JPAQueryFactory(entityManager);

        List<Expression> columns = constructProjections(input.getVesselLengthSegmentationType(), input.isGroupByCountry(), input.isGroupByPort(), input.isGroupByNuts2Region(),
                input.isGroupByNuts3Prefecture(), input.isGroupByFdiSubregion(), input.isGroupByFishingArea(), input.isGroupByGeoArea(), input.isGroupByGearType(), input.isGroupByLengthClass(),
                input.isGroupByAgeClass());

        BooleanBuilder whereClause = constructFilters(input.getYear(), input.getEvent(), input.getDurationStart(), input.getDurationEnd(), input.getNumOfVessels(), input.getNumOfVesselsBetween(),
                input.getOperatorNumOfVessels(), input.getGt(), input.getGtBetween(), input.getOperatorGt(), input.getKw(), input.getKwBetween(), input.getOperatorKw(),
                input.getAge(), input.getAgeBetween(), input.getOperatorAge(), input.getLength(), input.getLengthBetween(), input.getOperatorLength(), input.getCountries(),
                input.getPortNames(), input.getNuts2Regions(), input.getNuts3Prefectures(), input.getFdiSubregions(), input.getFishingAreas(), input.getGeoAreas(), input.getGears());

        JPAQuery<FleetAnalysisResult> query = qf.select(Projections.bean(FleetAnalysisResult.class, columns.toArray(new Expression<?>[0]))).from(qFleet);

        query = query.where(whereClause);

        if (input.isGroupByPort() || input.isGroupByNuts2Region() || input.isGroupByNuts3Prefecture() || input.isGroupByFdiSubregion() || input.isGroupByFishingArea()
                || input.isGroupByGeoArea() || input.getPortNames().length > 0 || input.getNuts2Regions().length > 0 || input.getNuts3Prefectures().length > 0
                || input.getFdiSubregions().length > 0 || input.getFishingAreas().length > 0 || input.getGeoAreas().length > 0)
            query = query.innerJoin(qpPorts).on(qFleet.placeOfRegistration.eq(qpPorts.portCodeFr).and(qFleet.country.eq(qpPorts.country)));

        query = query.groupBy(constructGroupBy(input).toArray(new Expression<?>[0]));

        // Define the composite expression for filtering as well:
        NumberExpression<Long> distinctVesselCount = Expressions.numberTemplate(
                Long.class,
                "count(DISTINCT CONCAT({0}, '-', {1}, '-', {2}))",
                qFleet.country,
                qFleet.year.stringValue(),
                qFleet.cfr
        );

        // Then add a HAVING clause based on the operator:
        if (input.getOperatorNumOfVessels() != null && input.getNumOfVessels() != null) {
            switch(input.getOperatorNumOfVessels()) {
                case EQUALS:
                    query.having(distinctVesselCount.eq(input.getNumOfVessels()));
                    break;
                case GREATER_THAN:
                    query.having(distinctVesselCount.gt(input.getNumOfVessels()));
                    break;
                case LESS_THAN:
                    query.having(distinctVesselCount.lt(input.getNumOfVessels()));
                    break;
                case BETWEEN:
                    query.having(distinctVesselCount.between(input.getNumOfVessels(), input.getNumOfVesselsBetween()));
                    break;
            }
        }

        return query.fetch();
    }

    private List<Expression> constructGroupBy(FleetAnalysisInput input) {
        List<Expression> expressions = new ArrayList<>();
        QFleetEuDecommissioned qFleet = QFleetEuDecommissioned.fleetEuDecommissioned;
        QPPortsEuFleetRegCircabcNut qpPorts = QPPortsEuFleetRegCircabcNut.pPortsEuFleetRegCircabcNut;
        expressions.add(qFleet.year);
        if (input.isGroupByCountry())
            expressions.add(qFleet.country);
        if (input.isGroupByPort()){
            expressions.add(qpPorts.portName);
            if (!expressions.contains(qFleet.country))
                expressions.add(qFleet.country);
        }
        if (input.isGroupByNuts2Region())
            expressions.add(qpPorts.nuts2Region);
        if (input.isGroupByNuts3Prefecture())
            expressions.add(qpPorts.nuts3Prefecture);
        if (input.isGroupByFdiSubregion())
            expressions.add(qpPorts.fdiSubregion);
        if (input.isGroupByFishingArea())
            expressions.add(qpPorts.fishingArea);
        if (input.isGroupByGeoArea())
            expressions.add(qpPorts.geoArea);
        if (input.isGroupByGearType())
            expressions.add(qFleet.gearType);
        if (input.isGroupByAgeClass())
            expressions.add(qFleet.ageClass);
        if (input.getEvent() != null && !input.getEvent().isBlank())
            expressions.add(qFleet.event);
        if (input.isGroupByLengthClass()) {
            if (input.getVesselLengthSegmentationType().equals("medbs"))
                expressions.add(qFleet.vlMedbs);
            else if (input.getVesselLengthSegmentationType().equals("otherwaters"))
                expressions.add(qFleet.vlOtherWaters);
            else if (input.getVesselLengthSegmentationType().equals("balticsea"))
                expressions.add(qFleet.vlBalticSea);
            else if (input.getVesselLengthSegmentationType().equals("gfcm"))
                expressions.add(qFleet.vlGfcm);
        }
        return expressions;
    }

    private List<Expression> constructProjections(String vesselLengthSegmentationType, boolean groupByCountry, boolean groupByPort, boolean groupByNuts2Region, boolean groupByNuts3Prefecture,
                                                  boolean groupByFdiSubregion, boolean groupByFishingArea, boolean groupByGeoArea, boolean groupByGearType,
                                                  boolean groupByLengthClass, boolean groupByAgeClass) {
        List<Expression> expressions = new ArrayList<>();
        QFleetEuDecommissioned qFleet = QFleetEuDecommissioned.fleetEuDecommissioned;
        QPPortsEuFleetRegCircabcNut qpPorts = QPPortsEuFleetRegCircabcNut.pPortsEuFleetRegCircabcNut;
        StringPath vesselLength = null;
        if (vesselLengthSegmentationType.equals("medbs"))
            vesselLength = qFleet.vlMedbs;
        else if (vesselLengthSegmentationType.equals("otherwaters"))
            vesselLength = qFleet.vlOtherWaters;
        else if (vesselLengthSegmentationType.equals("balticsea"))
            vesselLength = qFleet.vlBalticSea;
        else if (vesselLengthSegmentationType.equals("gfcm"))
            vesselLength = qFleet.vlGfcm;
        if (groupByLengthClass)
            expressions.add(vesselLength.as("lengthClass"));
        if (groupByNuts2Region)
            expressions.add(qpPorts.nuts2Region);
        if (groupByNuts3Prefecture)
            expressions.add(qpPorts.nuts3Prefecture);
        if (groupByFdiSubregion)
            expressions.add(qpPorts.fdiSubregion);
        if (groupByFishingArea)
            expressions.add(qpPorts.fishingArea);
        if (groupByGeoArea)
            expressions.add(qpPorts.geoArea);
        if (groupByGearType)
            expressions.add(qFleet.gearType);
        if (groupByAgeClass)
            expressions.add(qFleet.ageClass);
        if (groupByCountry)
            expressions.add(qFleet.country);
        if (groupByPort) {
            expressions.add(qpPorts.portName);
            if (!expressions.contains(qFleet.country))
                expressions.add(qFleet.country);
        }
        expressions.add(qFleet.year);
        expressions.add(qFleet.gt.doubleValue().sum().floor().as("sumGT"));
        expressions.add(qFleet.kw.doubleValue().sum().floor().as("sumKW"));
        NumberExpression avLength = roundToTwoDecimalPlaces(qFleet.loa.doubleValue().avg());
        expressions.add(avLength.as("averageVesselLength"));
        expressions.add(qFleet.vesselAge.doubleValue().avg().round().as("averageVesselAge"));
        NumberExpression<Long> distinctVesselCount = Expressions.numberTemplate(
                Long.class,
                "count(DISTINCT CONCAT({0}, '-', {1}, '-', {2}))",
                qFleet.country,
                qFleet.year.stringValue(),
                qFleet.cfr
        );
        expressions.add(distinctVesselCount.as("numberOfVessels"));
        return expressions;
    }

    private NumberTemplate roundToTwoDecimalPlaces(NumberExpression expression) {
        return numberTemplate(Double.class, "ROUND({0} * 100.0) / 100.0", expression);
    }

    private BooleanBuilder constructFilters(Short year, String event, Short durationStart, Short durationEnd,
                                            Long numOfVessels, Long numOfVesselsBetween, Integer operatorNumOfVessels, BigDecimal gt,
                                            BigDecimal gtBetween, Integer operatorGt, BigDecimal kw, BigDecimal kwBetween, Integer operatorKw,
                                            Short age, Short ageBetween, Integer operatorAge, BigDecimal length, BigDecimal lengthBetween,
                                            Integer operatorLength, String[] countries, String[] portNames, String[] nuts2Regions, String[] nuts3prefectures,
                                            String[] fdiSubregions, String[] fishingAreas, String[] geoAreas, String[] gears)
    {
        BooleanBuilder whereClause = new BooleanBuilder();
        QFleetEuDecommissioned qFleet = QFleetEuDecommissioned.fleetEuDecommissioned;
        QPPortsEuFleetRegCircabcNut qpPorts = QPPortsEuFleetRegCircabcNut.pPortsEuFleetRegCircabcNut;

        if (year != null)
            whereClause.and(qFleet.year.eq(year));
        else if (durationStart != null && durationEnd != null)
            whereClause.and(qFleet.year.goe(durationStart)).and(qFleet.year.loe(durationEnd));

        if (event != null && !event.isBlank())
            whereClause.and(qFleet.event.eq(event));

        /*if (operatorNumOfVessels == EQUALS)
            whereClause.and(qFleet.cfr.count().eq(numOfVessels));
        else if (operatorNumOfVessels == GREATER_THAN)
            whereClause.and(qFleet.cfr.count().gt(numOfVessels));
        else if (operatorNumOfVessels == LESS_THAN)
            whereClause.and(qFleet.cfr.count().lt(numOfVessels));
        else if (operatorNumOfVessels == BETWEEN)
            whereClause.and(qFleet.cfr.count().between(numOfVessels, numOfVesselsBetween));*/

        if (operatorGt == EQUALS)
            whereClause.and(qFleet.gt.eq(gt));
        else if (operatorGt == GREATER_THAN)
            whereClause.and(qFleet.gt.gt(gt));
        else if (operatorGt == LESS_THAN)
            whereClause.and(qFleet.gt.lt(gt));
        else if (operatorGt == BETWEEN)
            whereClause.and(qFleet.gt.between(gt,gtBetween));

        if (operatorKw == EQUALS)
            whereClause.and(qFleet.kw.eq(kw));
        else if (operatorKw == GREATER_THAN)
            whereClause.and(qFleet.kw.gt(kw));
        else if (operatorKw == LESS_THAN)
            whereClause.and(qFleet.kw.lt(kw));
        else if (operatorKw == BETWEEN)
            whereClause.and(qFleet.kw.between(kw,kwBetween));

        if (operatorAge == EQUALS)
            whereClause.and(qFleet.vesselAge.eq(age.shortValue()));
        else if (operatorAge == GREATER_THAN)
            whereClause.and(qFleet.vesselAge.gt(age.shortValue()));
        else if (operatorAge == LESS_THAN)
            whereClause.and(qFleet.vesselAge.lt(age.shortValue()));
        else if (operatorAge == BETWEEN)
            whereClause.and(qFleet.vesselAge.between(age.shortValue(),ageBetween.shortValue()));

        if (operatorLength == EQUALS)
            whereClause.and(qFleet.loa.eq(length));
        else if (operatorLength == GREATER_THAN)
            whereClause.and(qFleet.loa.gt(length));
        else if (operatorLength == LESS_THAN)
            whereClause.and(qFleet.loa.lt(length));
        else if (operatorLength == BETWEEN)
            whereClause.and(qFleet.loa.between(length, lengthBetween));

        if (countries != null && countries.length > 0 && !countries[0].isBlank()){
            BooleanBuilder countryClause = new BooleanBuilder();
            for (String country: countries) {
                countryClause.or(qFleet.country.eq(country));
            }
            whereClause.and(countryClause);
        }

        if (portNames != null && portNames.length > 0){
            BooleanBuilder portClause = new BooleanBuilder();
            for (String port: portNames) {
                portClause.or(qpPorts.portName.eq(port));
            }
            whereClause.and(portClause);
        }

        if (nuts2Regions != null && nuts2Regions.length > 0){
            BooleanBuilder nuts2Clause = new BooleanBuilder();
            for (String region: nuts2Regions) {
                nuts2Clause.or(qpPorts.nuts2Region.eq(region));
            }
            whereClause.and(nuts2Clause);
        }

        if (nuts3prefectures!= null && nuts3prefectures.length > 0){
            BooleanBuilder nuts3Clause = new BooleanBuilder();
            for (String prefecture: nuts3prefectures) {
                nuts3Clause.or(qpPorts.nuts3Prefecture.eq(prefecture));
            }
            whereClause.and(nuts3Clause);
        }

        if (fdiSubregions != null && fdiSubregions.length > 0){
            BooleanBuilder fdiClause = new BooleanBuilder();
            for (String region: fdiSubregions) {
                fdiClause.or(qpPorts.fdiSubregion.eq(region));
            }
            whereClause.and(fdiClause);
        }

        if (fishingAreas != null && fishingAreas.length > 0){
            BooleanBuilder fishingClause = new BooleanBuilder();
            for (String area: fishingAreas) {
                fishingClause.or(qpPorts.fishingArea.eq(area));
            }
            whereClause.and(fishingClause);
        }

        if (geoAreas != null && geoAreas.length > 0){
            BooleanBuilder geoClause = new BooleanBuilder();
            for (String area: geoAreas) {
                geoClause.or(qpPorts.geoArea.eq(area));
            }
            whereClause.and(geoClause);
        }

        if (gears != null && gears.length > 0) {
            BooleanBuilder gearClause = new BooleanBuilder();
            for (String gear: gears) {
                gearClause.or(qFleet.gearType.eq(gear));
            }
            whereClause.and(gearClause);
        }

        return whereClause;
    }
}

src/main/java/eu/medbsrdb/rdbfisapi/services/FleetVesselEuGearService.java
package eu.medbsrdb.rdbfisapi.services;

import eu.medbsrdb.rdbfisapi.entities.FleetVesselEuGear;
import eu.medbsrdb.rdbfisapi.entities.QFleetVesselEuGear;
import eu.medbsrdb.rdbfisapi.repositories.DataRepository;
import eu.medbsrdb.rdbfisapi.repositories.FleetVesselEuGearRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.Authentication;
import org.springframework.stereotype.Service;

import java.util.List;


@Service
public class FleetVesselEuGearService extends GenericService<FleetVesselEuGear, QFleetVesselEuGear> {
    @Autowired
    FleetVesselEuGearRepository repository;


    @Override
    public DataRepository getRepository() {
        return this.repository;
    }

    @Override
    public int saveAllJdbcBatch(List<FleetVesselEuGear> records, Authentication authentication) {
        return 0;
    }
}

src/main/java/eu/medbsrdb/rdbfisapi/services/FleetVesselEuService.java

package eu.medbsrdb.rdbfisapi.services;

import com.querydsl.core.types.OrderSpecifier;
import com.querydsl.core.types.Path;
import com.querydsl.core.types.Predicate;
import com.querydsl.core.types.dsl.Expressions;
import com.querydsl.core.types.dsl.PathBuilder;
import com.querydsl.jpa.impl.JPAQuery;
import eu.medbsrdb.rdbfisapi.advancedqueryparser.AdvancedQueryParser;
import eu.medbsrdb.rdbfisapi.entities.DcFdiACatch;
import eu.medbsrdb.rdbfisapi.entities.FleetVesselEu;
import eu.medbsrdb.rdbfisapi.entities.QDcFdiACatch;
import eu.medbsrdb.rdbfisapi.entities.QFleetVesselEu;
import eu.medbsrdb.rdbfisapi.repositories.DataRepository;
import eu.medbsrdb.rdbfisapi.repositories.FleetVesselEuRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.security.core.Authentication;
import org.springframework.stereotype.Service;

import javax.persistence.EntityManager;
import java.text.ParseException;
import java.util.List;


@Service
public class FleetVesselEuService extends GenericService<FleetVesselEu, QFleetVesselEu> {
    @Autowired
    FleetVesselEuRepository repository;


    @Override
    public DataRepository getRepository() {
        return this.repository;
    }

    @Override
    public int saveAllJdbcBatch(List<FleetVesselEu> records, Authentication authentication) {
        return 0;
    }
}

