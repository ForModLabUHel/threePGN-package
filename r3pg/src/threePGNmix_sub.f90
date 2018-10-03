! Translation of the D. Forrester code VBA https://sites.google.com/site/davidforresterssite/home/projects/3PGmix to Fortran
! V. Trotsiuk 
! Changes
!   `speciescounter` replaced with `spCount` - shorter
!   `speciescounter2` replaced with `spCount2`
!
!
! functions that need to be translated additionally
!   lookupTable

subroutine initialisation ()

    ! Assign the SWconst and SWpower parameters for this soil class
    if ( soilClass > 0 ) then        
        ! Standard soil type
        SWconst = 0.8 - 0.1 * soilClass
        SWpower = 11. - 2. * soilClass      
    elseIf ( soilClass < 0 ) then        
        ! Use supplied parameters
        SWconst = SWconst0
        SWpower = SWpower0      
    else      
        ! No soil-water effects
        SWconst = 999
        SWpower = SWpower0      
    end if
    
    ! Check fN(FR) for no effect: fNn = 0 ==> fN(FR)=1 for all FR
    ! ***DF this loops through for each species
    
    do spCount = 1, noSpecies ! ***DF
        
        if ( fNn(spCount) = 0 ) then             
            fN0(spCount) = 1        
        end if
        
        ! Derive some parameters
        
        pfsPower(spCount) = Log( pFS20(spCount) / pFS2(spCount) ) / Log( 20. / 2. )
        pfsConst(spCount) = pFS2(spCount) / 2. ** pfsPower(spCount)
        fCalphax(spCount) = fCalpha700(spCount) / (2 - fCalpha700(spCount))
        fCg0(spCount) = fCg700(spCount) / (2 * fCg700(spCount) - 1)
        
        ! Initialise ages
        MAIx(spCount) = 0
        LAIx(spCount) = 0
    
    end do ! ***DF
    
    ! Initial ASW must be between min and max ASW
    if (MinASW > MaxASW) then
        MinASW = MaxASW
    end if
    
    if (ASW <= MinASW) then
        ASW = MinASW
    else if (ASW >= maxASW) then
        ASW = maxASW
    end if
    
    poolFractn = Max(0.,min(1.,poolFractn))
    applIrrig = 0.
    pooledSW = 0.
    
end subroutine initialisation


subroutine getStandAge()

    integer :: latest
    
    standage = TimeInitialised
    startAge = int(TimeInitialised)
    
    ! ***DF now find the latest time a species is removed
    ! ***DF find the earliest startyear
    latest = -9999
    
    do spCount2 = 1, noSpecies ! ***DF
    
        if ( TimeAgeEnd(spCount2) > latest ) then       
            latest = TimeAgeEnd(spCount2)            
        end if
        
    end do ! ***DF
    
    Endage = int(latest) - 1 !***DF the roundup function has problems
    
    ! This procedure gets the starting month and intial stand age
    ! It is assumed that InitialYear and InitialMonth are the calendar year and month of the year
    ! at which the stand is initialised, and that PlantedYear and PlantedMonth are the calendar
    ! year and month of the year at which the stand was planted. If IntitialYear is less than
    ! PlantedYear, it is assumed to be referenced back to the PlantedYear
    ! Assign initial stand age
    
    if ( StartAge < 0 ) then        
        stop "Invalid age limits. The starting age must be greater than 0!"
    elseIf ( StartAge > Endage ) then   
        stop "Invalid age limits. The starting age is greater than the latest ending age!"       
    end if
    
end subroutine getStandAge


subroutine getAbsorbedPAR( month ) !***DF

    integer :: month
    double precision :: maxht, firstxaxisintercept, secondxaxisintercept
    
    ! ***DF maxht - the maximum height of a canopy layer
    ! firstxaxisintercept the relationship between solar zenith angle (y) and julian day (x) bounces of the x axis for lats lower than about 23 of so. This is used to adjust it (see Appendix B of Forrester et al., 2014, Forest Ecosystems, 1:17)
    
    secondxaxisintercept = 0.0018 * Lat ** 3 - 0.0031 * Lat ** 2 + 2.3826 * Lat + 266.62
    firstxaxisintercept = -0.0018 * Lat ** 3 + 0.0021 * Lat ** 2 - 2.3459 * Lat + 80.097

    !calculate the solar zenith angle for this latitude (lat) and month
    
    gamma(month) = 2 * Pi / 365 * ( julianday(month) - 1)
    declinationangle(month) = 0.006918 - (0.399912 * Cos(gamma(month))) + 0.070257 * Sin(gamma(month)) - 0.006758 * Cos(2 * gamma(month)) + 0.000907 * Sin(2 * gamma(month)) - 0.002697 * Cos(3 * gamma(month)) + 0.00148 * Sin(3 * gamma(month))
    szaprep(month) = Sin(Pi / 180 * Lat * -1) * Sin(declinationangle(month)) + Cos(Pi / 180 * Lat * -1) * Cos(declinationangle(month)) !VBA has no arccos!?
    solarzenithangle(month) = 180 / Pi * (Atn(-szaprep(month) / ((-szaprep(month) * szaprep(month) + 1) ** 0.5)) + 2 * Atn(1))
    
    if ( Lat >= 0 .and. Lat <= 23.4 ) then 
    !the zenith angle only needs to be adjusted if the lat is between about -23.4 and 23.4
        
        if ( julianday(month) > secondxaxisintercept .or. julianday(month) < firstxaxisintercept ) then
            adjSolarZenithAngle(month) = solarzenithangle(month) * -1
        end if
    
    else if ( Lat < 0 and. Lat >= -23.4 ) then
    
        if ( julianday(month) > firstxaxisintercept .and. julianday(month) < secondxaxisintercept ) then
            adjSolarZenithAngle(month) = solarzenithangle(month) * -1
        end if
        
    else
        adjSolarZenithAngle(month) = solarzenithangle(month)
    end if
    
    ! check that the height and LCL allometric equations have not predicted that height - LCL < 0
    ! and if so reduce LCL so that height - LCL = 0 (assumes height allometry is more reliable than LCL allometry)
    
    do spCount = 1, noSpecies
        if ( Height(spCount) - LCL(spCount) < 0 .and. & 
            alive(spCount) = .TRUE. .and. & 
            Crowndiameter(spCount) > 0 ) then
        
            LCL(spCount) = Height(spCount)
        end if
    end do
    
    !calculate species specific variables required for APAR calculations
    do spCount = 1, noSpecies
        Heightmidcrown(spCount) = LCL(spCount) / 2 + (Height(spCount) - LCL(spCount))
        Layercheck(spCount) = 0 !**DF just give all layerchecks a 0 to start with
        ! calculate the mean crown surface area and crown volume of each species
        
        if ( CrownShape(spCount) = 1 ) then !cone shaped
            CrownSA(spCount) = Pi * ((Crowndiameter(spCount) / 2) ** 2) + Pi * Crowndiameter(spCount) / 2 * (((Crowndiameter(spCount) / 2) ** 2) + LCL(spCount) ** 2) ** 0.5
            Crownvolume(spCount) = Pi * Crowndiameter(spCount) * Crowndiameter(spCount) * LCL(spCount) / 12
        else if ( CrownShape(spCount) = 2 ) then ! ellipsoid
            CrownSA(spCount) = 4 * Pi * ((((Crowndiameter(spCount) / 2) ** 1.6075) * ((Crowndiameter(spCount) / 2) ** 1.6075) + ((Crowndiameter(spCount) / 2) ** 1.6075) * ((LCL(spCount) / 2) ** 1.6075) + ((Crowndiameter(spCount) / 2) ** 1.6075) * ((LCL(spCount) / 2) ** 1.6075)) / 3) ** (1 / 1.6075)
            Crownvolume(spCount) = Pi * Crowndiameter(spCount) * Crowndiameter(spCount) * LCL(spCount) * 4 / 24
        else if ( CrownShape(spCount) = 3 ) then ! half-ellipsoid
            CrownSA(spCount) = Pi * ((Crowndiameter(spCount) / 2) ** 2) + (4 * Pi * ((((Crowndiameter(spCount) / 2) ** 1.6075) * ((Crowndiameter(spCount) / 2) ** 1.6075) + ((Crowndiameter(spCount) / 2) ** 1.6075) * ((LCL(spCount)) ** 1.6075) + ((Crowndiameter(spCount) / 2) ** 1.6075) * ((LCL(spCount)) ** 1.6075)) / 3) ** (1 / 1.6075)) / 2
            Crownvolume(spCount) = Pi * Crowndiameter(spCount) * Crowndiameter(spCount) * LCL(spCount) * 4 / 24
        else if (CrownShape(spCount) = 4 ) then ! rectangular
            CrownSA(spCount) = Crowndiameter(spCount) * Crowndiameter(spCount) * 2 + Crowndiameter(spCount) * LCL(spCount) * 4
            Crownvolume(spCount) = Crowndiameter(spCount) * Crowndiameter(spCount) * LCL(spCount)
        end if
    end do
    
    ! calculate the ratio of tree leaf area to crown surface area restrict kLS to 1
    do spCount = 1, noSpecies
        if ( alive(spCount) = .TRUE. ) then !if the species currently exists
            treeLAtoSAratio(spCount) = LAI(spCount) * 10000 / StemNo(spCount) / CrownSA(spCount)
        else
            treeLAtoSAratio(spCount) = 0
        end if
    end do
    
    ! allocate each species to a canopy layer
    ! find maximum height
    maxht = 0
    spCount = 1
    
    do spCount = 1, noSpecies
        if ( Height(spCount) > maxht ) then
            maxht = Height(spCount)
        end if
    end do
    
    maximumheight = maxht ! this is used later for the transpiration calculations
    
    ! all start with LayerForSpecies as 0
    do spCount = 1, noSpecies
        LayerForSpecies(spCount) = 0
    end do
    
    ! Also, each species that doesnt currently exist is given a layercheck of 1 to remove it from further consideration.
    do spCount = 1, noSpecies
        if ( alive(spCount) = False .or. Crowndiameter(spCount) = 0 ) then
            Layercheck(spCount) = 1
        end if
    end do
        
    ! Then each species with the max height is allocated to layer 1
    do spCount = 1, noSpecies
        if ( Height(spCount) = maxht ) then
            LayerForSpecies(spCount) = 1
            Layercheck(spCount) = 1
        end if
    end do

    ! Find the height to crown base of all species that had the maxht
    minLCH = maxht
    
    do spCount = 1, noSpecies
        if ( LayerForSpecies(spCount) = 1 .and. Height(spCount) - LCL(spCount) < minLCH ) then
            minLCH = Height(spCount) - LCL(spCount)
        end if
    end do

    ! put all species with height > minLCH into layer 1
    do spCount = 1, noSpecies
        if ( Height(spCount) > minLCH ) then
            LayerForSpecies(spCount) = 1
            Layercheck(spCount) = 1
        end if
    end do

    ! Put any species that doesn't currently exist into layer 1 so that it is ignored in the next calculations.
    do spCount = 1, noSpecies
        if ( alive(spCount) = False .or. Crowndiameter(spCount) = 0 ) then
            LayerForSpecies(spCount) = 1
        end if
    end do

    ! count the species already allocated to a layer
    Layercheckersum = 0
    do spCount = 1, noSpecies
        Layercheckersum = Layercheckersum + Layercheck(spCount)
    end do

    ! now allocate the rest of the species to a layer
    Layercheckersum2 = 0
    layer = 1
    nlayers = 1

    if ( Layercheckersum = noSpecies ) then
        Layercheckersum = noSpecies
    else !now repeat these things until all cohorts have been allocated a section
        do Layercheckersum, noSpecies
            ! find the minimun height to crown base of all species in the current layer, it starts from the previous minLCH
            do spCount = 1, noSpecies
                if ( LayerForSpecies(spCount) = layer .and. Height(spCount) - LCL(spCount) < minLCH .and. &
                alive(spCount) = .TRUE. .and. Crowndiameter(spCount) > 0 ) then
                    
                    minLCH = Height(spCount) - LCL(spCount)
                end if
            end do

            ! if new species have been added to the current layer, add any other species with height > minLCH
            do spCount = 1, noSpecies
                if ( Layercheck(spCount) = 0 .and. Height(spCount) > minLCH ) then 
                ! ignore species that have already been allocated to a layer
                    LayerForSpecies(spCount) = layer
                    Layercheck(spCount) = 1
                end if
            end do

            ! now count the species that are in a layer
            Layercheckersum2 = 0
            do spCount = 1, noSpecies
                Layercheckersum2 = Layercheckersum2 + Layercheck(spCount)
            end do

            if ( Layercheckersum2 > Layercheckersum ) then
                Layercheckersum = Layercheckersum2
            else
            ! it now needs a new minLCH, which is actually the max LCH of all species that have not yet been allocated to a layer
                minLCH = 0
                
                do spCount = 1, noSpecies
                    If ( LayerForSpecies(spCount) = 0 .and. Height(spCount) - LCL(spCount) > minLCH .and. &
                    alive(spCount) = .TRUE. .and. Crowndiameter(spCount) > 0 ) then
                        
                        minLCH = Height(spCount) - LCL(spCount)
                    end if
                end do
                
                ! now put the species with the new minLCH into the next layer
                do spCount = 1, noSpecies
                    if ( Height(spCount) - LCL(spCount) = minLCH .and. &
                    alive(spCount) = .TRUE. .and. Crowndiameter(spCount) > 0 ) then
                    
                        LayerForSpecies(spCount) = layer + 1
                    end if
                end do
                
                layer = layer + 1 ! only let it loop to the next layer if no more species were added
                
            end if
        
        end do
        
    end if
    
    nlayers = layer

    ! Now calculate the proportion of the canopy space that is filled by the crowns. The canopy space is the
    ! volume between the top and bottom of a layer that is filled by crowns in that layer. While doing this,
    ! save the top and bottom heights of each layer.

    do layer = 1, nlayers
        Variablesummer = 0
        maxht = 0 ! first find the top and bottom of this section
            
        do spCount = 1, noSpecies
            if ( LayerForSpecies(spCount) = layer .and. Height(spCount) > maxht ) then
                maxht = Height(spCount)
            end if
        end do
        
        TopHeightLayer(layer) = maxht

        minLCH = 1000
        
        do spCount = 1, noSpecies
            if ( LayerForSpecies(spCount) = layer .and. Height(spCount) - LCL(spCount) < minLCH .and. &
            alive(spCount) = .TRUE. .and. Crowndiameter(spCount) > 0 ) then
            
                minLCH = Height(spCount) - LCL(spCount)
            end if
        end do

        HeightCrownbaseLayer(layer) = minLCH

        ! now calculate the volume fraction of this section
        do spCount = 1, noSpecies
            if ( LayerForSpecies(spCount) = layer .and. alive(spCount) = .TRUE. .and. Crowndiameter(spCount) > 0 ) then
                if ( maxht - minLCH <= 0 ) then
                    Variablesummer = Variablesummer
                else
                    Variablesummer = Variablesummer + Crownvolume(spCount) * StemNo(spCount) / ((maxht - minLCH) * 10000)
                end if
            end if
        end do
        
        CanopyVolumefraction(layer) = Variablesummer
    
    end do


    ! determine the midpoints of all layers
    do layer = 1, nlayers
        HeightmidLayer(layer) = (TopHeightLayer(layer) - HeightCrownbaseLayer(layer)) / 2 + HeightCrownbaseLayer(layer)
    end do

    ! determine the ratio between the mid height of the given species and the mid height of the layer.
    do layer = 1, nlayers
        do spCount = 1, noSpecies
            if ( LayerForSpecies(spCount) = layer ) then
                HeightmidcrownPerHeightmidlayer(spCount) = Heightmidcrown(spCount) / HeightmidLayer(spCount)
            end if
        end do
    end do

    ! Calculate the sum of kL for all species in a layer
    do layer = 1, nlayers
        Variablesummer = 0
        do spCount = 1, noSpecies
            if ( LayerForSpecies(spCount) = layer ) then
                Variablesummer = Variablesummer + k(spCount) * LAI(spCount)
            end if
        end do
        LayerkLsum(layer) = Variablesummer
    end do
    
    !Constant to partition light between species and to account for vertical canopy heterogeneity (see Equations 2 and 3 of Forrester et al., 2014, Forest Ecosystems, 1:17)
    do layer = 1, nlayers
        do spCount = 1, noSpecies
            if ( LayerForSpecies(spCount) = layer ) then
                If ( noSpecies = 1 ) then 
                ! no adjustment is required for monocultures
                    lambdaV(spCount) = 1
                else if ( LayerkLsum(layer) = 0 ) then 
                ! there must not be any species with LAI because it is the dormant season for all existing species
                    lambdaV(spCount) = 0
                else
                    lambdaV(spCount) = 0.012306 + 0.236609 * k(spCount) * LAI(spCount) / LayerkLsum(layer) + 0.029118 * HeightmidcrownPerHeightmidlayer(spCount) + 0.608381 * k(spCount) * LAI(spCount) / LayerkLsum(layer) * HeightmidcrownPerHeightmidlayer(spCount)
                end if
            end if
        end do
    end do
    
    ! make sure the sum of all lambdaV = 1
    do layer = 1, nlayers
        Variablesummer = 0
        do spCount = 1, noSpecies
            if ( LayerForSpecies(spCount) = layer ) then
                Variablesummer = Variablesummer + lambdaV(spCount)
            end if
        end do
            
        if ( Variablesummer = 0 ) then 
        ! if no species had LAI because all were dormant
            do spCount = 1, noSpecies
                if ( LayerForSpecies(spCount) = layer ) then
                    AdjustedLambdaV(spCount) = 0
                end if
            end do
            
        else
            do spCount = 1, noSpecies
                if ( LayerForSpecies(spCount) = layer ) then
                    AdjustedLambdaV(spCount) = lambdaV(spCount) / Variablesummer
                end if
            end do
        end if
    end do
    
    ! Calculate the weighted kLS based on kL/sumkL
    do layer = 1, nlayers
        if ( LayerkLsum(layer) = 0 ) then 
        ! in the case where all species in the layer are dormant during the current month
            kLSweightedave(layer) = 0
        else
            Variablesummer = 0
            do spCount = 1, noSpecies
                if ( LayerForSpecies(spCount) = layer ) then
                    Variablesummer = Variablesummer + k(spCount) * treeLAtoSAratio(spCount) * k(spCount) * LAI(spCount) / LayerkLsum(layer)
                end if
            end do
            kLSweightedave(layer) = Variablesummer
        end if
    end do

    ! the kLS should not be greater than 1 (based on the data used to fit the light model in Forrester et al. 2014)
    ! This is because when there is a high k then LS is likely to be small.
    do layer = 1, nlayers
        if ( kLSweightedave(layer) > 1 ) then
            kLSweightedave(layer) = 1
        end if
    end do
    
    ! Constant to account for horizontal canopy heterogeneity such as gaps between trees and the change in zenith angle (and shading) with latitude and season (see Equations 2 and 5 of Forrester et al., 2014, Forest Ecosystems, 1:17)
    ! if the canopy volume fraction is < 0.01 (very small seedlings) then it is outside the range of the model there is no need for lambdaH so, make canopyvolumefraction = 0.01
    do layer = 1, nlayers
        do spCount = 1, noSpecies
            if ( LayerForSpecies(spCount) = layer ) then
                If ( adjSolarZenithAngle(month) <= 30 ) then
                    If ( CanopyVolumefraction(layer) < 0.01 ) then
                        lambdaH(spCount) = 0.8285 + ((1.09498 - 0.781928 * kLSweightedave(layer)) * 0.1 ** (0.01)) - 0.6714096 * 0.1 ** (0.01)
                    else
                        lambdaH(spCount) = 0.8285 + ((1.09498 - 0.781928 * kLSweightedave(layer)) * 0.1 ** (CanopyVolumefraction(layer))) - 0.6714096 * 0.1 ** (CanopyVolumefraction(layer))
                    end if
                else if ( adjSolarZenithAngle(month) > 30 ) then
                    if ( CanopyVolumefraction(layer) < 0.01 ) then
                        lambdaH(spCount) = 0.8285 + 0.00097 * 1.08259 ** adjSolarZenithAngle(month) + ((1.09498 - 0.781928 * kLSweightedave(layer)) * 0.1 ** (0.01)) - 0.6714096 * 0.1 ** (0.01)
                    else
                        lambdaH(spCount) = 0.8285 + 0.00097 * 1.08259 ** adjSolarZenithAngle(month) + ((1.09498 - 0.781928 * kLSweightedave(layer)) * 0.1 ** (CanopyVolumefraction(layer))) - 0.6714096 * 0.1 ** (CanopyVolumefraction(layer))
                    end if
                end if
            end if
        end do
    end do


    ! The absorbed PAR for the given canopy layer
    abovePAR = SolarRad * daysInMonth(month)       !MJ m-2 month-1 (RAD in 3PGpjs)

    do layer = 1, nlayers
        layerAPAR = abovePAR * (1 - 2.71828182845905 ** (-LayerkLsum(layer)))
        if ( layerAPAR < 0 ) then
            APARByLayer(layer) = 0
            layerAPAR = 0
        else
            APARByLayer(layer) = layerAPAR
        end if
        abovePAR = abovePAR - layerAPAR
    end do

    
    !The absorbed PAR for the given species
    do layer = 1, nlayers
        do spCount = 1, noSpecies
            if ( LayerForSpecies(spCount) = layer ) then
                !***DF this used to have month in it but this whole sub is run each month so month is now redundant here.
                APARBySpecies(spCount) = APARByLayer(layer) * lambdaH(spCount) * AdjustedLambdaV(spCount) 
            end if
        end do
    end do

    !The proportion of above canopy PAR absorbed by each species. This is used for net radiation calculations in the gettranspiration sub
    fitotal = 0
    do spCount = 1, noSpecies
        fi(spCount) = APARBySpecies(spCount) / (SolarRad * daysInMonth(month))
        fitotal = fitotal + fi(spCount)
    end do
    
    ! rename for consistency with 3-PGpjs 2.7 names
    do spCount = 1, noSpecies
        RADint(spCount) = APARBySpecies(spCount)
    end do

    ! calculate the LAI above the given species for within canopy VPD calculations
    ! calculate LAI of each layer
    do layer = 1, nlayers
        layerLAI(layer) = 0
        do spCount = 1, noSpecies
            if ( LayerForSpecies(spCount) = layer ) then
                layerLAI(layer) = layerLAI(layer) + LAI(spCount)
            end if
        end do
    end do

    ! now calculate the LAI of all layers above and part of the current layer if the species is in the lower half of the layer
    do spCount = 1, noSpecies
        LAIabove(spCount) = 0
        do layer = 1, nlayers
            if ( LayerForSpecies(spCount) < layer ) then
                LAIabove(spCount) = LAIabove(spCount) + layerLAI(layer)
            else if ( LayerForSpecies(spCount) = layer .and. HeightmidcrownPerHeightmidlayer(spCount) < 1 ) then
                LAIabove(spCount) = LAIabove(spCount) + (1 - HeightmidcrownPerHeightmidlayer(spCount)) * layerLAI(layer)
            end if
        end do
    end do

end subroutine getAbsorbedPAR


subroutine getMortality( oldN, oldW) 
! V.T. This maybe can be rather the function, than subroutine.
! ***DF changed some to arrays
! This function determines the number of stems to remove to ensure the
! self-thinning rule is satisfied. It applies the Newton-Rhapson method
! to solve for N to an accuracy of 1 stem or less. To change this,
! change the value of "accuracy".
    
    integer :: i
    double precision :: oldN, oldW, fN, dfN, dN, n, x1, x2, accuracy
    
    accuracy = 1 / 1000.
    n = oldN / 1000.
    
    x1 = 1000 * mS(spCount2) * oldW / oldN
    
    i = 0
    do
        i = i + 1
        ! ensure that n is not negative
        
        if ( n <= 0 ) then
            n = 0
        else
            x2 = wSx1000(spCount2) * n ** (1. - thinPower(spCount2))
            fN = x2 - x1 * n - (1. - mS(spCount2)) * oldW
            dfN = (1. - thinPower(spCount2)) * x2 / n - x1
            dN = -fN / dfN
            n = n + dN
        end if
        
        if ( Abs(dN) <= accuracy .or. i >= 5) exit
    end do
    
    getMortality = oldN - 1000.0 * n
    
end subroutine getMortality
 
subroutine getDiameterDistributions() 
! this is also for ws distributions

    double precision :: Varx, Ex
    
    ! Diameter distributions are used to correct for bias when calculating pFS from mean dbh, and ws distributions are
    ! used to correct for bias when calculating mean dbh from mean ws. This bias is caused by Jensen's inequality and is
    ! corrected using the approach described by Duursma and Robinson (2003) FEM 186, 373-380, which uses the CV of the
    ! distributions and the exponent of the relationship between predicted and predictor variables.
    
    ! The default is to ignore the bias. The alternative is to correct for it by using empirically derived weibull distributions
    ! from the weibull parameters provided by the user. If the weibull distribution does not vary then just provide scale0 and shape0.
    
    do spCount = 1, noSpecies
        if ( alive(spCount) = .TRUE. ) then
            if ( DbhandwsDistributionType = "none" ) then 
                ! ignore the bias
                DrelBiaspFS(spCount) = 0
                DrelBiasheight(spCount) = 0
                DrelBiasBasArea(spCount) = 0
                DrelBiasLCL(spCount) = 0
                DrelBiasCrowndiameter(spCount) = 0
                wsrelBias(spCount) = 0
            else if ( DbhandwsDistributionType = "provided" ) then 
                !correct for bias using the weibull parameters provided as input parameters.
                ! These use dbh, height, age and competition of the previous time step (except for the first time step), that has
                ! already been corrected for bias. If they use the biased values from the current time step they will lead to biased
                ! predictions of weibull parameters.
                
                ! if the species is still zero then remove it from the equation
                if ( SpeciesAge(spCount) = 0 ) then
                    DWeibullScale(spCount) = Exp(Dscale0(spCount) + DscaleB(spCount) * Log(avDBH(spCount)) + Dscalerh(spCount) * Log(relativeheight(spCount)) + Dscalet(spCount) * 0 + DscaleC(spCount) * Log(totalCompetition))
                    DWeibullShape(spCount) = Exp(Dshape0(spCount) + DshapeB(spCount) * Log(avDBH(spCount)) + Dshaperh(spCount) * Log(relativeheight(spCount)) + Dshapet(spCount) * 0 + DshapeC(spCount) * Log(totalCompetition))
                else
                    DWeibullScale(spCount) = Exp(Dscale0(spCount) + DscaleB(spCount) * Log(avDBH(spCount)) + Dscalerh(spCount) * Log(relativeheight(spCount)) + Dscalet(spCount) * Log(SpeciesAge(spCount)) + DscaleC(spCount) * Log(totalCompetition))
                    DWeibullShape(spCount) = Exp(Dshape0(spCount) + DshapeB(spCount) * Log(avDBH(spCount)) + Dshaperh(spCount) * Log(relativeheight(spCount)) + Dshapet(spCount) * Log(SpeciesAge(spCount)) + DshapeC(spCount) * Log(totalCompetition))
                end if
                
                ! if the location parameters are not provided then predict them
                if ( Dlocation0(spCount) = 0 .and. DlocationB(spCount) = 0 .and. Dlocationrh(spCount) = 0 .and. Dlocationt(spCount) = 0 .and. DlocationC(spCount) = 0 ) then
                    DWeibullLocation(spCount) = Maximum(0.01, (Int(avDBH(spCount)) / 1 - 1 - DWeibullScale(spCount) * gammadistribution(1 + 1 / DWeibullShape(spCount))))
                else
                    DWeibullLocation(spCount) = Maximum(0.01, Exp(Dlocation0(spCount) + DlocationB(spCount) * Log(avDBH(spCount)) + Dlocationrh(spCount) * Log(relativeheight(spCount)) + Dlocationt(spCount) * Log(SpeciesAge(spCount)) + DlocationC(spCount) * Log(totalCompetition)))
                end if
                
                Ex = DWeibullLocation(spCount) + DWeibullScale(spCount) * gammadistribution(1 + 1 / DWeibullShape(spCount))
                ! now convert the Ex from weibull scale to actual scale of diameter units in cm
                Varx = DWeibullScale(spCount) ** 2 * (gammadistribution(1 + 2 / DWeibullShape(spCount)) - gammadistribution(1 + 1 / DWeibullShape(spCount)) ** 2)
                CVdbhDistribution(spCount) = Varx ** 0.5 / Ex
                DrelBiaspFS(spCount) = 0.5 * (pfsPower(spCount) * (pfsPower(spCount) - 1)) * CVdbhDistribution(spCount) ** 2
                DrelBiasheight(spCount) = 0.5 * (nHB(spCount) * (nHB(spCount) - 1)) * CVdbhDistribution(spCount) ** 2
                DrelBiasBasArea(spCount) = 0.5 * (2 * (2 - 1)) * CVdbhDistribution(spCount) ** 2
                DrelBiasLCL(spCount) = 0.5 * (nHLB(spCount) * (nHLB(spCount) - 1)) * CVdbhDistribution(spCount) ** 2
                DrelBiasCrowndiameter(spCount) = 0.5 * (nKB(spCount) * (nKB(spCount) - 1)) * CVdbhDistribution(spCount) ** 2
                
                !prevent unrealisticly large bias, by restricting it to within + or - 50%
                if ( DrelBiaspFS(spCount) > 0.5 ) then
                    DrelBiaspFS(spCount) = 0.5
                else if ( DrelBiaspFS(spCount) < -0.5 ) then
                    DrelBiaspFS(spCount) = -0.5
                end if
                    
                if ( DrelBiasheight(spCount) > 0.5 ) then
                    DrelBiasheight(spCount) = 0.5
                else if ( DrelBiasheight(spCount) < -0.5 ) then
                    DrelBiasheight(spCount) = -0.5
                end if

                if ( DrelBiasBasArea(spCount) > 0.5 ) then
                    DrelBiasBasArea(spCount) = 0.5
                else if ( DrelBiasBasArea(spCount) < -0.5 ) then
                    DrelBiasBasArea(spCount) = -0.5
                end if
                    
                if ( DrelBiasLCL(spCount) > 0.5 ) then
                    DrelBiasLCL(spCount) = 0.5
                else if ( DrelBiasLCL(spCount) < -0.5 ) then
                    DrelBiasLCL(spCount) = -0.5
                end if
                    
                if ( DrelBiasCrowndiameter(spCount) > 0.5 ) then
                    DrelBiasCrowndiameter(spCount) = 0.5
                else if ( DrelBiasCrowndiameter(spCount) < -0.5 ) then
                    DrelBiasCrowndiameter(spCount) = -0.5
                end if
                    
                if ( wsrelBias(spCount) > 0.5 ) then
                    wsrelBias(spCount) = 0.5
                else if ( wsrelBias(spCount) < -0.5 ) then
                    wsrelBias(spCount) = -0.5
                end if
                
                !if the species is still zero then remove it from the equation
                
                if ( SpeciesAge(spCount) = 0 ) then
                    wsWeibullScale(spCount) = Exp(wsscale0(spCount) + wsscaleB(spCount) * Log(avDBH(spCount)) + wsscalerh(spCount) * Log(relativeheight(spCount)) + wsscalet(spCount) * 0 + wsscaleC(spCount) * Log(totalCompetition))
                    wsWeibullShape(spCount) = Exp(wsshape0(spCount) + wsshapeB(spCount) * Log(avDBH(spCount)) + wsshaperh(spCount) * Log(relativeheight(spCount)) + wsshapet(spCount) * 0 + wsshapeC(spCount) * Log(totalCompetition))
                else
                    wsWeibullScale(spCount) = Exp(wsscale0(spCount) + wsscaleB(spCount) * Log(avDBH(spCount)) + wsscalerh(spCount) * Log(relativeheight(spCount)) + wsscalet(spCount) * Log(SpeciesAge(spCount)) + wsscaleC(spCount) * Log(totalCompetition))
                    wsWeibullShape(spCount) = Exp(wsshape0(spCount) + wsshapeB(spCount) * Log(avDBH(spCount)) + wsshaperh(spCount) * Log(relativeheight(spCount)) + wsshapet(spCount) * Log(SpeciesAge(spCount)) + wsshapeC(spCount) * Log(totalCompetition))
                end if

                ! if the location parameters are not provided then predict them
                if ( wslocation0(spCount) = 0 .and. wslocationB(spCount) = 0 .and. wslocationrh(spCount) = 0 .and. wslocationt(spCount) = 0 .and. wslocationC(spCount) = 0 ) then
                    wsWeibullLocation(spCount) = Maximum(0.01, (Int(AvStemMass(spCount)) / 10 - 1 - wsWeibullScale(spCount) * gammadistribution(1 + 1 / wsWeibullShape(spCount)))) ! The /10 is the width of the ws classes (10 kg)
                else
                    wsWeibullLocation(spCount) = Maximum(0.01, Exp(wslocation0(spCount) + wslocationB(spCount) * Log(avDBH(spCount)) + wslocationrh(spCount) * Log(relativeheight(spCount)) + wslocationt(spCount) * Log(SpeciesAge(spCount)) + wslocationC(spCount) * Log(totalCompetition)))
                end if
                
                Ex = wsWeibullLocation(spCount) + wsWeibullScale(spCount) * gammadistribution(1 + 1 / wsWeibullShape(spCount))
                Varx = wsWeibullScale(spCount) ** 2 * (gammadistribution(1 + 2 / wsWeibullShape(spCount)) - gammadistribution(1 + 1 / wsWeibullShape(spCount)) ** 2)
                CVwsDistribution(spCount) = Varx ** 0.5 / Ex
                wsrelBias(spCount) = 0.5 * (1 / nWs(spCount) * (1 / nWs(spCount) - 1)) * CVwsDistribution(spCount) ** 2 !***DF the nWS is replaced with 1/nWs because the equation is inverted to predict dbh from ws, instead of ws from dbh
                    
                ! prevent unrealisticly large bias, by restricting it to within + or - 50%
                if ( wsrelBias(spCount) > 0.5 ) then
                    wsrelBias(spCount) = 0.5
                else if ( wsrelBias(spCount) < -0.5 ) then
                    wsrelBias(spCount) = -0.5
                end if
            
            end if

            ! now remove the bias from DBH and the variables that are derived from it
            avDBH(spCount) = (AvStemMass(spCount) / aWs(spCount)) ** (1 / nWs(spCount)) * (1 + wsrelBias(spCount))
            asArea(spCount) = ((((avDBH(spCount) / 200) ** 2) * Pi) * StemNo(spCount)) * (1 + DrelBiasBasArea(spCount))
            Height(spCount) = (aH(spCount) * avDBH(spCount) ** nHB(spCount) * totalCompetition ** nHC(spCount)) * (1 + DrelBiasheight(spCount))
            LCL(spCount) = (aHL(spCount) * avDBH(spCount) ** nHLB(spCount) * totalLAI ** nHLL(spCount) * totalCompetition ** nHLC(spCount) * relativeheight(spCount) ** nHLrh(spCount)) * (1 + DrelBiasLCL(spCount))
            Crowndiameter(spCount) = (aK(spCount) * avDBH(spCount) ** nKB(spCount) * Height(spCount) ** nKH(spCount) * totalCompetition ** nKC(spCount) * relativeheight(spCount) ** nKrh(spCount)) * (1 + DrelBiasCrowndiameter(spCount))
          
            !now remove the bias from pFS
            pFS(spCount) = (pfsConst(spCount) * avDBH(spCount) ** pfsPower(spCount)) * (1 + DrelBiaspFS(spCount))

        end if
    end do
end subroutine getDiameterDistributions


subroutine getCropTrees() 
! This is used to calculate the stand variables for crop trees as opposed to the whole stand

    integer :: croptreecounter
    double precision :: sizeclass, sizeclassDBH, Nclass, HeightTemp
    
    ! croptreecounter - counts the number of crop trees in the diameter classes already considered when
    ! determining the smallest diameter class containing crop trees
    ! sizeclass - the current diameter class
    ! sizeclassDBH - the dbh of the current size class
    ! Nclass - the number of trees in a given diameter class
    ! HeightTemp a temporary height used for volume calculations

    do spCount = 1, noSpecies
        !only continue with this if crop trees have been requested and if the location parameter (for weibull
        ! distributions) is more than 0.01. This is because location cannot be negative when bias is being
        ! corrected but if it is restricted to be >=0 then unrealistic crop tree values will be predicted
        ! until the location parameter would have been >0 without the restriction.
        if ( CropTrees(spCount) > 0 .and. DWeibullLocation(spCount) > 0.01 ) then
            ! Find the largest size class with at least 1 tree, but first find the smallest size class with at least 1 tree.
            sizeclass = 1
            if ( sizeclass < DWeibullLocation(spCount) ) then
                Nclass = 0
            else
                Nclass = (DWeibullShape(spCount) / DWeibullScale(spCount) * (((sizeclass - DWeibullLocation(spCount)) / DWeibullScale(spCount)) ** (DWeibullShape(spCount) - 1)) * Exp(-1 * ((sizeclass - DWeibullLocation(spCount)) / DWeibullScale(spCount)) ** DWeibullShape(spCount))) * StemNo(spCount)
            end if
            
            if (Nclass >= 1 ) then
                do while ( Nclass >= 1 ) ! V.T. Please check original was Do Until Nclass < 1
                    sizeclass = sizeclass + 1
                    if ( sizeclass < DWeibullLocation(spCount) ) then
                        Nclass = 0
                    else
                        Nclass = (DWeibullShape(spCount) / DWeibullScale(spCount) * (((sizeclass - DWeibullLocation(spCount)) / DWeibullScale(spCount)) ** (DWeibullShape(spCount) - 1)) * Exp(-1 * ((sizeclass - DWeibullLocation(spCount)) / DWeibullScale(spCount)) ** DWeibullShape(spCount))) * StemNo(spCount)
                    end if
                end do
                sizeclass = sizeclass - 1
            else
            ! firstly find the minimum tree size
                sizeclass = 1
                if ( sizeclass < DWeibullLocation(spCount) ) then
                    Nclass = 0
                else
                    Nclass = (DWeibullShape(spCount) / DWeibullScale(spCount) * (((sizeclass - DWeibullLocation(spCount)) / DWeibullScale(spCount)) ** (DWeibullShape(spCount) - 1)) * Exp(-1 * ((sizeclass - DWeibullLocation(spCount)) / DWeibullScale(spCount)) ** DWeibullShape(spCount))) * StemNo(spCount)
                end if
                
                do while (Nclass < 1) ! V.T. originally Do Until Nclass >= 1
                    sizeclass = sizeclass + 1
                    if ( sizeclass < DWeibullLocation(spCount) ) then
                        Nclass = 0
                    else
                        Nclass = (DWeibullShape(spCount) / DWeibullScale(spCount) * (((sizeclass - DWeibullLocation(spCount)) / DWeibullScale(spCount)) ** (DWeibullShape(spCount) - 1)) * Exp(-1 * ((sizeclass - DWeibullLocation(spCount)) / DWeibullScale(spCount)) ** DWeibullShape(spCount))) * StemNo(spCount)
                    end if
                end do
                
                !now find the largest size class
                do while (Nclass >= 1) ! V.T. Do Until Nclass < 1
                    sizeclass = sizeclass + 1
                    if ( sizeclass < DWeibullLocation(spCount) ) then
                        Nclass = 0
                    Else
                        Nclass = (DWeibullShape(spCount) / DWeibullScale(spCount) * (((sizeclass - DWeibullLocation(spCount)) / DWeibullScale(spCount)) ** (DWeibullShape(spCount) - 1)) * Exp(-1 * ((sizeclass - DWeibullLocation(spCount)) / DWeibullScale(spCount)) ** DWeibullShape(spCount))) * StemNo(spCount)
                    end if
                end do
                
                sizeclass = sizeclass - 1
            end if
            
            ! Now loop back until the number of trees counted is equal to X (note that only variables
            ! that are predicted from B (diameter) can be considered here)
            avDBHCrop(spCount) = 0
            StandVolCrop(spCount) = 0
            BasAreaCrop(spCount) = 0
            WSCrop(spCount) = 0
            HeightCrop(spCount) = 0

            if ( StemNo(spCount) <= CropTrees(spCount) ) then 
            !all trees are then crop trees
                avDBHCrop(spCount) = avDBH(spCount)
                StandVolCrop(spCount) = StandVol(spCount)
                BasAreaCrop(spCount) = BasArea(spCount)
                WSCrop(spCount) = WS(spCount)
                HeightCrop(spCount) = Height(spCount)
            else
                croptreecounter = 0
                Do while ( croptreecounter < CropTrees(spCount) .or. sizeclass >= 1 ) ! V.T. Do Until croptreecounter >= CropTrees(spCount) Or sizeclass < 1
                    if ( sizeclass < DWeibullLocation(spCount) ) then
                        Nclass = 0
                    else
                        Nclass = (DWeibullShape(spCount) / DWeibullScale(spCount) * (((sizeclass - DWeibullLocation(spCount)) / DWeibullScale(spCount)) ** (DWeibullShape(spCount) - 1)) * Exp(-1 * ((sizeclass - DWeibullLocation(spCount)) / DWeibullScale(spCount)) ** DWeibullShape(spCount))) * StemNo(spCount)
                    end if
                    
                    croptreecounter = croptreecounter + Nclass
            
                    avDBHCrop(spCount) = avDBHCrop(spCount) + Nclass * sizeclass
                    BasAreaCrop(spCount) = BasAreaCrop(spCount) + Nclass * sizeclass * sizeclass * Pi / 40000
                    WSCrop(spCount) = WSCrop(spCount) + Nclass * aWs(spCount) * sizeclass ** nWs(spCount) / 1000
                    HeightTemp = (aH(spCount) * sizeclass ** nHB(spCount) * totalCompetition ** nHC(spCount))
                    HeightCrop(spCount) = HeightCrop(spCount) + Nclass * HeightTemp
                    StandVolCrop(spCount) = StandVolCrop(spCountspCount) + Nclass * (aV(spCount) * sizeclass ** nVB(spCount) * HeightTemp ** nVH(spCount) * (sizeclass * sizeclass * HeightTemp) ** nVBH(spCount))
        
                    sizeclass = sizeclass - 1
                end do
            end if

            ! remove any extra trees that were in the smallest sizeclass with crop trees
            if ( croptreecounter > CropTrees(spCount) ) then
                do while (croptreecounter /= CropTrees(spCount)) !!! Do Until croptreecounter = CropTrees(spCount)
                    croptreecounter = croptreecounter - 1
                    avDBHCrop(spCount) = avDBHCrop(spCount) - 1 * sizeclass
                    BasAreaCrop(spCount) = BasAreaCrop(spCount) - 1 * sizeclass * sizeclass * Pi / 40000
                    WSCrop(spCount) = WSCrop(spCount) - 1 * aWs(spCount) * sizeclass ** nWs(spCount) / 1000
                    HeightTemp = (aH(spCount) * sizeclass ** nHB(spCount) * totalCompetition ** nHC(spCount))
                    HeightCrop(spCount) = HeightCrop(spCount) - 1 * HeightTemp
                    StandVolCrop(spCount) = StandVolCrop(spCount) - 1 * (aV(spCount) * sizeclass ** nVB(spCount) * HeightTemp ** nVH(spCount) * (sizeclass * sizeclass * HeightTemp) ** nVBH(spCount))
                end do
            end if
            
            ! convert DBH and height to means rather than sums
            avDBHCrop(spCount) = avDBHCrop(spCount) / CropTrees(spCount)
            HeightCrop(spCount) = HeightCrop(spCount) / CropTrees(spCount)
            
        end if
        ! if CropTrees(spCount) was not > 0 then dont do any calculations for this species
    end do

end subroutine getCropTrees


subroutine getConductance() 
! The LAI is scaled according to species proportions by LAI.

    double precision :: tanh !Visual Basic for Applications has no tanh function. This is currently not active, but might be used in future
    ! to allow for a non-linear relationship between LAI and gc
    
    if (Apply3PGpjsPhysMod = .TRUE. ) then 
    ! Apply the original 3PGpjs calculation (but with the scaled LAI for mixtures).
        do spCount = 1, noSpecies
            if ( alive(spCount) = .TRUE. ) then
                if ( LAIMix(spCount) <= LAIgcx(spCount) ) then
                    CanCond(spCount) = (MinCond(spCount) + (MaxCond(spCount) - MinCond(spCount)) * LAIMix(spCount) / LAIgcx(spCount)) * SpeciesProportionLAI(spCount) * PhysMod(spCount) * fCg(spCount)
                else
                    CanCond(spCount) = MaxCond(spCount) * PhysMod(spCount) * fCg(spCount) * SpeciesProportionLAI(spCount)
                end if
            else
                CanCond(spCount) = 0
            end if
        end do
    else
    ! instead of the above where PhysMod = min(fWS, fVPD), use PhysMod = fWS x fVPD, and also add fTgc
        do spCount = 1, noSpecies
            if ( alive(spCount) = .TRUE. ) then
                !This tanh part is currently not active, but can be used to allow for a non-linear relationship between LAI and gc
                !tanh = (Exp((LAIMix(spCount) / LAIgcx(spCount))) - Exp(-(LAIMix(spCount) / LAIgcx(spCount)))) / (Exp((LAIMix(spCount) / LAIgcx(spCount))) + Exp(-(LAIMix(spCount) / LAIgcx(spCount)))) fTgc(spCount)
                !CanCond(spCount) = MaxCond(spCount) * fSW * fVPD(spCount) * fAge(spCount) * fTgc(spCount) * minimum(1, tanh) * SpeciesProportionLAI(spCount)
                if ( LAIMix(spCount) <= LAIgcx(spCount) ) then
                    CanCond(spCount) = (MinCond(spCount) + (MaxCond(spCount) - MinCond(spCount)) * LAIMix(spCount) / LAIgcx(spCount)) * SpeciesProportionLAI(spCount) * fSW * fVPD(spCount) * fTgc(spCount) * fAge(spCount) * fCg(spCount)
                else
                    CanCond(spCount) = MaxCond(spCount) * fSW * fVPD(spCount) * fTgc(spCount) * fAge(spCount) * fCg(spCount) * SpeciesProportionLAI(spCount)
                end if
            else
                CanCond(spCount) = 0
            end if
        end do
    end if
end subroutine getConductance


subroutine getTranspiration( month )
! Penman-Monteith equation for computing canopy transpiration
! in kg/m2/day, which is converted to mm/day.
! The following are constants in the PM formula (Landsberg & Gower, 1997)
    
    integer :: month
    double precision :: e20, rhoAir, lambda, VPDconv
    
    e20 = 2.2          ! rate of change of saturated VP with T at 20C
    rhoAir = 1.2       ! density of air, kg/m3
    lambda = 2460000  ! latent heat of vapourisation of H2O (J/kg)
    VPDconv = 0.000622 ! convert VPD to saturation deficit = 18/29/1000
            
    if ( Apply3PGpjswaterbalance = .TRUE. ) then
        TotalTransp = 0
        do spCount = 1, noSpecies
            if ( alive(spCount) = .TRUE. ) then
                netrad = Qa + Qb * (SolarRad * 10 ** 6 / DayLength)                ! SolarRad in MJ/m2/day --> W/m2
                defTerm(spCount) = rhoAir * lambda * (VPDconv * VPD) * BLcond(spCount)
                div(spCount) = CanCond(spCount) * (1 + e20) + BLcond(spCount)
                Transp(spCount) = daysInMonth(month) * CanCond(spCount) * (e20 * netrad + defTerm(spCount)) / div(spCount) / lambda * DayLength      ! in J/m2/s then the "/lambda*h" converts to kg/m2/day and the days in month then coverts this to kg/m2/month
                TotalTransp = TotalTransp + Transp(spCount)
            end if
        end do
    else
        TotalTransp = 0
        do spCount = 1, noSpecies
            if ( alive(spCount) = .TRUE. ) then
                ! the within canopy ra and VPDspecies have been calculated using information from the light submodel
                ! and from the calculation of the modifiers. The netrad for each species is calculated
                ! using the fi (proportion of PAR absorbed by the given species) and is calculated by the light submodel.
                ! Now get Transp
                netrad = (Qa + Qb * (SolarRad * 10 ** 6 / DayLength)) * fi(spCount)              ! SolarRad in MJ/m2/day ---> * 10^6 J/m2/day ---> /daylength converts to only daytime period ---> W/m2
                defTerm(spCount) = rhoAir * lambda * (VPDconv * VPDspecies(spCount)) / ra(spCount)
                div(spCount) = CanCond(spCount) * (1 + e20) + 1 / ra(spCount)
                Transp(spCount) = daysInMonth(month) * CanCond(spCount) * (e20 * netrad + defTerm(spCount)) / div(spCount) / lambda * DayLength       ! in J/m2/s then the "/lambda*h" converts to kg/m2/day and the days in month then coverts this to kg/m2/month
                TotalTransp = TotalTransp + Transp(spCount)
            end if
        end do
        
        if ( totalLAI > 0 ) then
            spCount = spCount - 1 ! V.T. Do we need the `-1` here??
            ! now get the soil evaporation (soil ra = 5 * totalLAI, and VPD of soil = VPD * Exp(totalLAI * -Log(2) / 5))
            netrad = (Qa + Qb * (SolarRad * 10 ** 6 / DayLength)) * (1 - fitotal)            ! SolarRad in MJ/m2/day ---> * 10^6 J/m2/day ---> /daylength converts to only daytime period ---> W/m2
            defTerm(spCount) = rhoAir * lambda * (VPDconv * (VPD * Exp(totalLAI * -Log(2) / 5))) / (5 * totalLAI)
            SoilCond = MaxSoilCond * ASW / MaxASW ! the default MaxSoilCond is 0.0025
            div(spCount) = SoilCond * (1 + e20) + 1 / (5 * totalLAI)
            SoilEvap = daysInMonth(month) * SoilCond * (e20 * netrad + defTerm(spCount)) / div(spCount) / lambda * DayLength       ! in J/m2/s then the "/lambda*h" converts to kg/m2/day and the days in month then coverts this to kg/m2/month
            TotalTransp = TotalTransp + SoilEvap
        end if
    end if
end subroutine getTranspiration


subroutine gammaFoliage( age ) 
! ***DF changed to arrays
! V.T. Can be probably a function

    double precision :: age, kgammaF

    if ( tgammaF(spCount2) * gammaF1(spCount2) = 0 ) then
        gammaFoliage = gammaF1(spCount2)
    else
        kgammaF = 12 * Log(1 + gammaF1(spCount2) / gammaF0(spCount2)) / tgammaF(spCount2)
        gammaFoliage = gammaF1(spCount2) * gammaF0(spCount2) / (gammaF0(spCount2) + (gammaF1(spCount2) - gammaF0(spCount2)) * Exp(-kgammaF * age))
    end if
end subroutine gammaFoliage

subroutine isDormantSeason( currentmonth, spCount ) 
! This is called if the leafgrow parameter is not 0, and hence the species is decidous
! This is true if "currentmonth" is part of the dormant season
! V.T. can be rather a function

    integer :: currentmonth, spCount
    
    if ( leafgrow(spCount) > leaffall(spCount) ) then
        if ( currentmonth >= leaffall(spCount) .and. currentmonth <= leafgrow(spCount) ) then
            isDormantSeason = .TRUE.
        end if
    else if ( leafgrow(spCount) < leaffall(spCount) ) then
        if ( currentmonth <= leafgrow(spCount) .or. currentmonth >= leaffall(spCount) ) then
            isDormantSeason = .TRUE.
        end if
    end if
end subroutine isDormantSeason


subroutine getTotalsAndProportions() 
! calculate species proportions and stand totals
! first get the totals (and relativeheight)

    Cummulativeheight = 0
    totalBA = 0
    totalLAI = 0
    totalStemNo = 0
    totalCompetition = 0
    Meanstandheight = 0
    
    do spCount = 1, noSpecies
        totalBA = totalBA + avDBH(spCounter) * avDBH(spCounter) / 4 * Pi * StemNo(spCounter) / 10000
        totalLAI = totalLAI + LAI(spCounter)
        totalStemNo = totalStemNo + StemNo(spCounter)
        totalCompetition = totalCompetition + Density(spCounter) * avDBH(spCounter) * avDBH(spCounter) / 4 * Pi * StemNo(spCounter) / 10000
        Cummulativeheight = Cummulativeheight + Height(spCounter) * StemNo(spCounter)
        
        if ( totalStemNo <= 0 ) then
            Meanstandheight = 0
        else
            Meanstandheight = Cummulativeheight / totalStemNo
        end if
    end do

    if ( totalStemNo <= 0 ) then 
    ! ***DF it is ok for some but not all species to be removed (a stand cannot contain zero species)
        stop "No trees. The stem number has become zero. Run terminated."
    end if

    ! now that the Meanstandheight (weighted by StemNo) has been calculated, calculate the relative height of each species
    do spCount = 1, noSpecies
        if ( Meanstandheight <= 0 ) then
            relativeheight(spCounter) = 0
        else
            relativeheight(spCounter) = Height(spCounter) / Meanstandheight
        end if
    end do

    ! now get the proportions and adjust variables for mixtures
    do spCount = 1, noSpecies !***DF
        if ( alive(spCounter) = .TRUE. .and. avDBH(spCounter) > 0 .and. LAI(spCounter) > 0 ) then
            SpeciesProportionBA(spCounter) = (avDBH(spCounter) * avDBH(spCounter) / 4 * Pi * StemNo(spCounter) / 10000) / totalBA
            SpeciesProportionLAI(spCounter) = LAI(spCounter) / totalLAI
        
            if ( SpeciesProportionBA(spCounter) > 0 .and. SpeciesProportionBA(spCounter) < 0.01 ) then
                SpeciesProportionBA(spCounter) = 0.01 ! very small proportions lead to unrealistic StemNoMix or LAIMix, which are only used for mortality and light partitioning and are therefore not influenced by very low values.
            end if
            
            StemNoMix(spCounter) = StemNo(spCounter) / SpeciesProportionBA(spCounter)
            LAIMix(spCounter) = LAI(spCounter) / SpeciesProportionLAI(spCounter)
       
        else if ( alive(spCounter) = .TRUE. .and. avDBH(spCounter) > 0 ) then 
        ! If there are trees but it is the dormant season then the LAI for those species is excluded
            SpeciesProportionBA(spCounter) = (avDBH(spCounter) * avDBH(spCounter) / 4 * Pi * StemNo(spCounter) / 10000) / totalBA
            
            if ( SpeciesProportionBA(spCounter) > 0 .and. SpeciesProportionBA(spCounter) < 0.01 ) then
                SpeciesProportionBA(spCounter) = 0.01  ! Very small proportions lead to unrealistic StemNoMix or LAIMix, which are only used for mortality and light partitioning and are therefore not influenced by very low values.
            end if
            
            StemNoMix(spCounter) = StemNo(spCounter) / SpeciesProportionBA(spCounter)
            SpeciesProportionLAI(spCounter) = 0
            LAIMix(spCounter) = 0
       
        else
            SpeciesProportionBA(spCounter) = 0
            StemNoMix(spCounter) = 0
            SpeciesProportionLAI(spCounter) = 0
            LAIMix(spCounter) = 0
        end if
    end do

end subroutine getTotalsAndProportions

subroutine doThinning( n )
! V.T. Here we need to check the thinning. Since in the r3PGN we assign thinning in the table
! as `site_thinning` and here each stock (R, F, S) have it separate thinning info

    integer :: n
    double precision :: delN
    
    ! If it is time to do a thinning, carry out the thinning (if there are
    ! stems to remove) and update the thinnning eventNo
    
    if ( SpeciesAge(spCount2) >= thinAges(spCount2, n) ) then   
     
        if ( StemNo(spCount2) > thinVals(spCount2, n) ) then       
            delN = ( StemNo(spCount2) - thinVals(spCount2, n)) / StemNo(spCount2)
            StemNo(spCount2) = StemNo(spCount2) * (1 - delN)            
            !if the stand is thinned from above, then the ratios (F, R and S) of stem, foliage and roots to be removed
            ! relative to the mean tree in the stand will be >1. If the product of this ratio and delN is > 1 then the
            ! new WF, WR or WS will be < 0, which is impossible.
            ! Therefore, make sure this is >= 0.  
                  
            if ( delN * thinWF(spCount2, n) > 1 ) then
                WF(spCount2) = 0
                StemNo(spCount2) = 0
            else
                WF(spCount2) = WF(spCount2) * (1 - delN * thinWF(spCount2, n))
            end if
            
            if ( delN * thinWR(spCount2, n) > 1 ) then
                WR(spCount2) = 0
                StemNo(spCount2) = 0
            else
                WR(spCount2) = WR(spCount2) * (1 - delN * thinWR(spCount2, n))
            end if
            
            if ( delN * thinWS(spCount2, n) > 1 ) then
                WS(spCount2) = 0
                StemNo(spCount2) = 0
            else
                WS(spCount2) = WS(spCount2) * (1 - delN * thinWS(spCount2, n))
            end if
            
            AvStemMass(spCount2) = WS(spCount2) * 1000 / StemNo(spCount2) !an update of AvStemMass is required for volume calculations
        end if
        
    n = n + 1
    
    end if

end subroutine doThinning


subroutine doDefoliation( n )

    integer :: n
    
    ! If it is time of a defoliation, carry out the defoliation
    ! and update the defoliation eventNo
    
    if ( SpeciesAge(spCount2) >= defolAges(spCount2, n) ) then
        WF(spCount2) = WF(spCount2) * defolVals(spCount2, n)
        n = n + 1
    end if
    
end subroutine doDefoliation


subroutine doFertilising( n )

    integer :: n
    
    ! If it is time of a change in FR, carry out the change
    ! and update the FR eventNo
    
    if ( SpeciesAge(spCount2) >= FRages(spCount2, n) ) then
        FR(spCount2) = FRVals(spCount2, n)
        n = n + 1
    end if
    
end subroutine doDefoliation


subroutine doMonteCarlo() 
! V.T. I have some concerns about it, but I think we also will not include this into the model

    ! To use this activate the lines between the ########### after the line containing "Call doThinning"
    ! and enter an annual probability of disturbance.
    
    double precision :: delN !this is the probability that a given tree size or species will be affected
    
    ! This is not part of 3PG so to prevent the need for parameters that describe the probability of breakage, just identify species
    ! by one of their parameters in this study I just fit them as 2-parameter weibull functions
    
    if ( k(spCount2) = 0.66 ) then ! must be species 2
        delN = ((2.2 / 24) * ( ( int(avDBH(spCount2)) / 24) ** (2.2 - 1)) * ( exp(-((int(avDBH(spCount2)) / 24) ** 2.2)))) * 14.5
    else if ( k(spCount2) = 0.2921 ) then ! must be species 3    
        delN = ((2.2 / 24) * ( ( int(avDBH(spCount2)) / 24) ** (2.2 - 1)) * ( exp(-((int(avDBH(spCount2)) / 24) ** 2.2)))) * 14
    else if ( k(spCount2) = 0.6507 ) then ! must be species 5   
        delN = ((2.2 / 24.5) * ( ( int(avDBH(spCount2)) / 24.5) ** (2.2 - 1)) * ( exp(-((int(avDBH(spCount2)) / 24.5) ** 2.2)))) * 14.5        
    end if
    
    
    StemNo(spCount2) = StemNo(spCount2) * (1 - delN)
    WF(spCount2) = WF(spCount2) * (1 - delN) !* thinWF(spCount2, n))
    WR(spCount2) = WR(spCount2) * (1 - delN) !* thinWR(spCount2, n))
    WS(spCount2) = WS(spCount2) * (1 - delN) !* thinWS(spCount2, n))
    
end subroutine doMonteCarlo


subroutine assignSilviculturalEvents( age )
! V.T. please check the allocation and deallocation
! `lookupTable` is a custom function from Maths_routines module in D.F.

    integer :: i, n, nV, nV2
    double precision :: age, x
    double precision, dimension(:), allocatable :: ages, vals
    character :: name 
    
    !Assign values to silvicultural events
    ! the age is relative to the whole stand start age
    ! these are done at the total stand level not the species level
    age = standage 
    
    if ( nMinASW > 0 ) then
        
        nV = 0
        nV2 = -1 ! this is required otherwise the MinASW is provided one period too early '***DF
        
        allocate ( ages(nMinASW) )
        allocate ( vals(nMinASW) )
        
        do i = 1, nMinASW
        
            nV = nV + 1
            nV2 = nV2 + 1
            ages(i) = minASWages(nV)
            
            if ( interpolateLookups = False ) then
            
                if ( nV = nMinASW ) then                
                    vals(i) = minASWVals(nV)                  
                else if ( nV = 1 ) then               
                    vals(i) = minASWVals(nV)                
                else                
                    vals(i) = minASWVals(nV2)
                end if
            
            else            
                vals(i) = minASWVals(nV)            
            end if
            
        end do
        
        MinASW = lookupTable(age, nMinASW, ages, vals)

        deallocate ( ages )
        deallocate ( vals )
        
    else if ( nIrrig > 0 ) then
    
        nV = 0
        nV2 = -1 !this is required otherwise the applied irrigation is provided one period too early '***DF
        
        allocate ( ages(nIrrig) )
        allocate ( vals(nIrrig) )
        
        do i = 1, nIrrig
            
            nV = nV + 1
            nV2 = nV2 + 1
            ages(i) = irrigAges(nV)
            
            if ( interpolateLookups = False ) then
            
                if ( nV = nIrrig ) then                
                    vals(i) = irrigVals(nV)                    
                else if ( nV = 1 ) then               
                    vals(i) = irrigVals(nV)               
                else              
                    vals(i) = irrigVals(nV2)
                end if
            
            else                 
                vals(i) = irrigVals(nV)   
            end if
            
        end do
        
        applIrrig = lookupTable(age, nIrrig, ages, vals)
        
        deallocate ( ages )
        deallocate ( vals )
        
    end if

end subroutine assignSilviculturalEvents


subroutine assignFertilisingData( age )

    ! Change the FR if necessary
    
    integer :: i, n, nV, nV2
    double precision :: age, x
    double precision, dimension(:), allocatable :: ages, vals
    character :: name 
    
    if ( nFR = 0 ) exit 
    
    nV = 0
    nV2 = -1 !this is required otherwise the FR is provided one period too early '***DF
    
    allocate ( ages(nFR) )
    allocate ( vals(nFR) )

    do i = 1, nFR
        
        nV = nV + 1
        nV2 = nV2 + 1
        ages(i) = FRages(spCount2, nV)
        
        if (interpolateLookups = False) then
        
            if ( nV = nFR ) then
                vals(i) = FRVals(spCount2, nV)                
            else if ( nV = 1 ) then            
                vals(i) = FRVals(spCount2, nV)            
            else               
                vals(i) = FRVals(spCount2, nV2)                
            end if
            
        else        
            vals(i) = FRVals(spCount2, nV)        
        end if
        
    end do
    
    FR(spCount2) = lookupTable(age, nFR, ages, vals)
    deallocate ( ages )
    deallocate ( vals )    

end subroutine assignFertilisingData


subroutine assignVaryData( age )
! Assign values to Vary block data

    integer :: i, n, nV, nV2
    double precision :: age, x
    double precision, dimension(:), allocatable :: ages, vals
    character :: name 
    
    if ( nVary = 0 ) exit 
    
    nV = 0
    
    do n = 1, nVary
    
        name = varyNames(n)
        allocate ( ages(varyNos(n)) )
        allocate ( vals(varyNos(n)) )
        
        do i = 1, varyNos(n)
        
            nV = nV + 1
            ages(i) = varyAges(spCount2, nV) !***DF now a 2D array
        
            if ( interpolateLookups = False ) then
        
                if ( i = varyNos(n) ) then          
                    vals(i) = varyVals(spCount2, nV)              
                else if ( i = 1 ) then          
                    vals(i) = varyVals(spCount2, nV)               
                else           
                    vals(i) = varyVals(spCount2, nV - 1)               
                end if
            
            else        
            vals(i) = varyVals(spCount2, nV) !***DF now a 2D array        
            end if
        
        end do
    
        x = lookupTable(age, varyNos(n), ages, vals)
    
!!!! V.T this functions are not translated yet
!        Call setSiteFactor(name, x)
!        if kwClass <> kwSiteFactor then Call setParameter(name, x)
    
    end do
    
end subroutine assignVaryData


subroutine isOutputDate( n ) 
! This function checks to see if the current stand age corresponds to an output date.
! Note that if it does, the counter for opAges is incremented

    integer :: n, y, m
    logical :: b
    double precision :: opAge
    
    if ( noopAges <= 0 ) then        
        b = .TRUE.
    else if ( n <= noopAges ) then    
        !Call parseAge(opAges(n), y, m) '***DF
        opAge = opAges(n) ! y + m / 12  *** DF
        b = Abs(standage - opAge) < 0.01
        
        if b then n = n + 1
        
    else       
        b = False       
    end if
    
    isOutputDate = b
    
end subroutine isOutputDate
