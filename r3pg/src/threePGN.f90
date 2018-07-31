! This is the main Fortran script for running 3PG model.
! Script have the following structure:
!   Variables and Parameters declaration
!   ....
! 
! Developed: Minnuno F.
! Commented: Trotsiuk V. 
! 
! Edits:
! 2018.08.01 
!   Commenting
!   Replacing `tab` with `space` as Fortran set warnings to it
!   Consider renaming the input variables of the model to fit the one in R function

subroutine model(y, &
                nMonths, &
                noOfSites, &
                nClimID, &
                nvariables, &
                vars, &
                siteData, &
                totThinning, &
                thinningInputs, &
                weather, &
                pa)

    implicit none

! ----------------------------------------------------------------------------------------
! Declare input variables

    integer :: nvariables, nMonths, noOfSites, totThinning, nClimID, vars(nvariables)
    real(kind=8), dimension(totThinning, 6) :: thinningInputs
    real(kind=8), dimension(noOfSites, 16) ::  siteData
    real(kind=8), dimension(nmonths, 5, nClimID) :: weather
    real(kind=8), dimension(noOfSites+46) :: pa
    real(kind=8) :: y(nmonths, nvariables, noOfSites)
    real(kind=8) :: outs(59)
    
    real(kind=8), dimension(noOfSites) :: climID
    real(kind=8), dimension (noOfSites) :: number_site
    character(8), dimension (noOfSites) :: name_site
    integer :: ii, siteNo, month, startMonth, jj, ij,ijj
    real(kind=8), dimension(12) :: NEPmat=0
    real(kind=8) :: aNEP
    real(kind=8), dimension(noOfSites) :: Lat_site, StemNo_site, ASW_site, MinASW_site, & 
        MaxASW_site, FR_site, poolFractn_site, startAge_site, WF_i_site,WR_i_site,WS_i_site, &
        startMonth_site, SoilClass_site, endAge_site !(noOfSites)
    integer, dimension(noOfSites) :: nThinning_site
    real(kind=8), dimension(12) :: daysInMonth = (/31.,28.,31.,30.,31.,30.,31.,31.,30.,31.,30.,31./), &
        dayOfYear = (/16.,44.,75.,105.,136.,166.,197.,228.,258.,289.,319.,350./)

    real(kind=8) :: Pi = 3.141592654, ln2 = 0.693147181
    real (kind=8) :: Lat
    integer :: i
    real(kind=8) :: SLAt 
    real(kind=8) :: cLat
    real(kind=8) :: sinDec
    real(kind=8) :: cosH0
    real(kind=8), dimension(nmonths) :: fDayLength
    real(kind=8) :: MoistRatio
    real(kind=8) :: RelAge
    real(kind=8) :: alphaC
    real(kind=8) :: m

!   The following are constants in the PM formula (Landsberg & Gower, 1997)
    real(kind=8), parameter :: e20 = 2.2          ! rate of change of saturated VP with T at 20C
    real(kind=8), parameter :: rhoAir = 1.2       ! density of air, kg/m3
    real(kind=8), parameter :: lambda = 2460000   ! latent heat of vapourisation of H2O (J/kg)
    real(kind=8), parameter :: VPDconv = 0.000622 ! convert VPD to saturation deficit = 18/29/1000
    real(kind=8) :: netRad, defTerm, div, Etransp

!   Climate input
    real(kind=8), dimension (nmonths) :: Tav, SolarRad, VPD, Tn, Tx, Rain, VPDx, VPDn, RainDays, FrostDays, DayLength

!   fT - Temperature dependent modifier
    real(kind=8) :: fT, Tmin, Topt, Tmax

!   fVPD - VPD dependent modifier
    real(kind=8) :: fVPD, CoeffCond

!   fSW - Soil water dependent modifier
    real(kind=8) :: fSW, ASW, SWconst, SWpower,SoilClass

!   fNutr -  Nutrition dependent modifier
    real(kind=8) :: fNutr, fN0, FR, fNn

!   fCalpha - CO2 dependent modifier for quantum efficiency
    real(kind=8) :: fCalpha

!   fFrost - Frost dependent modifier
    real(kind=8) :: fFrost, kF

!   CO2 modifier
    real(kind=8) :: fCalpha700, fCalphax, fCg0, fCg700

!   fAge - Age dependent modifier
    real(kind=8) :: fAge, MaxAge, rAge, nAge, StandAge

!   PhysMod - Physiological modifier of canopy conductance
    real(kind=8) :: PhysMod

!   Light interception, production and respiration
    real(kind=8) :: CanCover, fullCanAge, lightIntcptn, k, LAI

!   Calculate PAR, APAR, APARu, GPP and NPP
    real(kind=8) :: RAD, PAR, molPAR_MJ, APAR, APARu, alpha, GPPmolc, GPPdm, gDM_mol, NPP, WF, WS, WR, TotalW, &
        WF_month, WS_month, WR_month, epsilon, GPP, CO2, fCg, RADint

!   Respiration parameters
    real(kind=8) :: rg, rf, rw, Q10

!   derived parameters
    real(kind=8) :: pfsPower, pfsConst

!   Carbon allocation routine and mensurational data
    real(kind=8) :: m0, pFS20, pFS2, AvStemMass, avDBH, StemConst, StemPower, BasArea, Height, aH, bW, &
        cD, SLA, SLA1, SLA0, tSLA, avLAI, pFS, pR, pRx, pRn, pS, pF, fracBB0, fracBB1, tBB, Density, &
        StemNo, fracBB, StandVol, MAI, aV, nVB, nVN, oldVol, rho0,rho1, tRho, CVI

!   Calculate soil carbon balance
!   Mortality, litterfall and turnover, self thinning law        
    real(kind=8) :: incrWF, incrWR, incrWS, Littfall, gammaF1, gammaF0, tgammaF, gammaR, kgammaF, &
        delStems, mS, mF, mR, WL, wSx1000, thinPower, n, x1, x2, fN, gammaN, gammaN1, gammaN0, tgammaN, &
        ngammaN, getMortality, accuracy, delStemNo, dfN, dN, j, wSmax,gammaF, gammaFoliage, lossWF, &
        lossWR, mortality, FoliageMort, RootMort, StemMort, selfThin

!   Soil carbon and nitrogen balances
    real(kind=8) :: kr, krmax, kl, klmax, ko, komax, Yl_Coutflux, hc, Yl_C, Yr_Coutflux, Yr_C, &
        humification_l, humification_r, O_Coutflux, O_C, Yl_Noutflux, el, Yl_N, qbc, humification_N_l, &
        qh, humification_N_r, Yr_N, Yr_Noutflux, er, O_Noutflux, O_N, qir, qil, Ncf, TotalCarbo, TotalNitro

!   Water balance
    real(kind=8) :: CanCond, MaxCond, LAIgcx, Qa, qb, BLcond, gC, CanopyTranspiration, Transp, &
        LAImaxIntcptn, MaxIntcptn, EvapTransp, Irrig, cumIrrig, MinASW, MaxASW, poolFractn, MinCond, &
        pooledSW, fRainInt,RainInt, excessSW, RunOff, supIrrig,TranspScaleFactor, WUE

!   Carbon fluxes (MG/ha)
    real(kind=8) :: GPP_C, NPP_C, Raut, Rhet, Reco, dmC, NEP

!   Systematic error for height, DBH and NEP in the Bayesian Calibration
    real(kind=8) :: ESYSH, ESYSDBH, ESYSNEP

!   Thinning
    integer :: nThinning,countThinning
    real(kind=8) :: delN
    real(kind=8), allocatable, dimension(:,:) :: thinning
    real(kind=8), allocatable, dimension(:,:) :: site_thinning

!   Management aspects
!   Stand caratheristics
    real(kind=8) :: WF_i
    real(kind=8) :: WS_i
    real(kind=8) :: WR_i

!   Site caractheristics
    real(kind=8) :: Yr_C_i
    real(kind=8) :: Yl_C_i
    real(kind=8) :: O_C_i
    real(kind=8) :: Yr_N_i
    real(kind=8) :: Yl_N_i
    real(kind=8) :: O_N_i


! ----------------------------------------------------------------------------------------
!   Read the site data
    number_site = siteData(:,1)
    lat_site = siteData(:,2)
    StemNo_site = siteData(:,3)
    SoilClass_site = siteData(:,4)
    ASW_site = siteData(:,5)
    MinASW_site = siteData(:,6)
    MaxASW_site = siteData(:,7)
    poolFractn_site = siteData(:,8)
    startAge_site = siteData(:,9)
    endAge_site = siteData(:,10)
    startMonth_site = siteData(:,11)
    WF_i_site = siteData(:,12)
    WR_i_site = siteData(:,13)
    WS_i_site = siteData(:,14)
    nThinning_site = siteData(:,15)
    climID = siteData(:,16)

!   Add thinning if present
    if (totThinning > 0) then
        allocate(thinning(totThinning,6))
        thinning = thinningInputs
    end if


! ----------------------------------------------------------------------------------------
!   Assign parameters
    StemPower = pa ( 1 )
    aH = pa ( 2 )
    bW = pa ( 3 )
    klmax = pa ( 4 )
    alpha = pa ( 5 )
    Tmin = pa ( 6 )
    krmax = pa ( 7 )
    gammaF1 = pa ( 8 )
    fN0 = pa ( 9 )
    rg = pa ( 10 )
    rho1 = pa ( 11 )
    gammaR = pa ( 12 )
    Topt = pa ( 13 )
    MaxCond = pa ( 14 )
    StemConst = pa ( 15 )
    pFS20 = pa ( 16 )
    pRn = pa ( 17 )
    k = pa ( 18 )
    fracBB1 = pa (19 )
    LAIgcx = pa (20 )
    fullCanAge = pa (21 )
    pRx = pa (22 )
    CoeffCond = pa (23 )
    pFS2 = pa (24 )
    hc = pa (25 )
    kF = pa (26 )
    SLA1 = pa (27 )
    tBB = pa (28 )
    m0 = pa (29 )
    tSLA = pa (30 )
    Tmax = pa (31 )
    MaxIntcptn = pa (32 )
    fracBB0 = pa (33 )
    SLA0 = pa (34 )
    BLcond = pa (35 )
    nAge = pa (36 )
    tgammaF = pa (37 )
    MaxAge = pa (38 )
    rAge = pa (39 )
    gammaF0 = pa (40 )
    komax = pa (41 )
    dmC = pa (42 )
    Yl_C_i = pa (43 )
    Yr_C_i = pa (44 )
    O_C_i = pa (45 )
    LAImaxIntcptn = pa (46 )
    
    do siteNo = 1, noOfSites
        FR_site(siteNo) = pa ( 46 + siteNo )
    enddo
    
!   Assign constant
    fCalpha700 = 1.4
    fCg700 = 0.7
    fNn = 1
    gammaN1 = 0
    gammaN0 = 0
    tgammaN = 0
    ngammaN = 1
    wSx1000 = 300
    thinPower = 1.5
    mF = 0
    mR = 0.2
    mS = 0.2
    MinCond = 0
    rho0 = 0.45
    tRho = 0
    aV = 0
    nVB = 0
    nVN = 0
    Qa = -90
    Qb = 0.8
    gDM_mol = 24
    molPAR_MJ = 2.3
    CO2 = 350
    Yr_N_i = 0
    Yl_N_i = 2
    O_N_i = 6
    qir = 300
    qil = 21.9
    qh = 29
    qbc = 10
    el = 0.2
    er = 0.2
    Ncf = 1.75


! ----------------------------------------------------------------------------------------
!   Initiate the loop over sites

    do siteNo = 1, noOfSites

        Yr_C = Yr_C_i
        Yl_C = Yl_C_i
        O_C = O_C_i
        Yr_N = Yr_N_i
        Yl_N = Yl_N_i
        O_N = O_N_i

!       Initialize
        Irrig = 0.0
        Lat = Lat_site ( siteNo )
        StemNo = StemNo_site ( siteNo )
        SoilClass = SoilClass_site ( siteNo )
        FR = FR_site ( siteNo )
        ASW = ASW_site ( siteNo )
        MinASW = MinASW_site ( siteNo )
        MaxASW = MaxASW_site ( siteNo )
        poolFractn = poolFractn_site( siteNo )
        standAge = startAge_site( siteNo )
        startMonth = startMonth_site( siteNo )
        WF = WF_i_site( siteNo )
        WR = WR_i_site( siteNo )
        WS = WS_i_site( siteNo )
        nThinning = nThinning_site( siteNo )

!       Site level thinning if preset
        if (nThinning > 0) then
            countThinning = 1
            allocate( site_thinning( nThinning,6))
            ij = 0
            do jj = 1, totThinning
                if (thinning(jj,6) == siteNo) then
                    ij = ij+1
                    site_thinning(ij,:) = thinning(jj,:)
                end if
            end do
        end if

!       Assign SWconstant and SWpower as function of Soil Class
        SWconst = 0.8 - 0.1 * SoilClass
        SWpower = 11. - 2. * SoilClass

        MAI = 0.
        If (fNn == 0.) then 
            fN0 = 1.
        end if

        fCalphax = fCalpha700 / (2. - fCalpha700)
        fCg0 = fCg700 / (2. *fCg700 - 1.)
!       initial ASW must be between min and max asw
        if (MinASW > MaxASW) then
            MinASW = MaxASW
        end if

        if (ASW <= MinASW) then
            ASW = MinASW
            else if (ASW >= maxASW) then
            ASW = maxASW
        end if

        poolFractn = Max(0.,min(1.,poolFractn))
        Irrig = 0. !to check
        pooledSW = 0.
        LAI = 0.

! -----------------------------------------------
! Climate
!       Initialise climate for the site
        do ii = 1, nmonths
            Tn(ii) = weather(ii,2,int(climID(siteNo)))
            Tx(ii) = weather(ii,1,int(climID(siteNo)))
            SolarRad(ii) = weather(ii,4,int(climID(siteNo)))
            Rain(ii) = weather(ii,3,int(climID(siteNo)))
            FrostDays(ii) = weather(ii,5,int(climID(siteNo)))
        end do

!       Calculate climate variables  
        do ii = 1, nmonths
            Tav(ii) = 0.5 * (Tn(ii) + Tx(ii))
            VPDx(ii) = 6.1078 * Exp(17.269 * Tx(ii) / (237.3 + Tx(ii)))
            VPDn(ii) = 6.1078 * Exp(17.269 * Tn(ii) / (237.3 + Tn(ii)))
            VPD(ii) = 0.5 * (VPDx(ii) - VPDn(ii))
        end do


! -----------------------------------------------
! Age dependent factors
        SLA = SLA1 + (SLA0 - SLA1) * Exp(-ln2 * (StandAge / tSLA) ** 2.)
        fracBB = fracBB1 + (fracBB0 - fracBB1) * Exp(-ln2 * (StandAge / tBB))
        Density = rho1 + (rho0 - rho1) * Exp(-ln2 * (StandAge / tRho))
        If (tgammaF * gammaF1 == 0) Then
            gammaFoliage = gammaF1
        Else
            kgammaF = 12 * Log(1 + gammaF1 / gammaF0) / tgammaF
            gammaFoliage = gammaF1 * gammaF0 / (gammaF0 + (gammaF1 - gammaF0) * Exp(-kgammaF * StandAge))
        End If

        gammaF = gammaFoliage

        pfsPower = Log(pFS20 / pFS2) / Log(20./2.)
        pfsConst = pFS2 / 2. ** pfsPower
        AvStemMass = WS * 1000. / StemNo                                 !kg/tree
        avDBH = StemConst*(((WS+WF) * 1000. )/ StemNo) ** (StemPower) !(AvStemMass/StemConst) ** (1/StemPower) !
        BasArea = (((avDBH / 200.) ** 2.) * Pi) * StemNo
        Height = aH*((WS + WF) * 1000. / StemNo)**bW !* StemNo ** cD
        LAI = WF * SLA * 0.1
        avLAI = LAI

        if (aV > 0) then
            StandVol = aV * avDBH ** nVB * StemNo ** nVN 
        else
            StandVol = WS * (1 - fracBB) / Density  
        end if

        oldVol = StandVol  


! ----------------------------------------------------------------------------------------
!   Loop through each month of the data

        month=startMonth
        do ii = 1, nmonths

            month = month + 1
            if (month > 12) then
                month = 1
            end if

!           gets fraction of day when sun is "up"
            SLAt = Sin(Pi * Lat / 180.)
            cLat = Cos(Pi * Lat / 180.)
            sinDec = 0.4 * Sin(0.0172 * (dayOfYear(month) - 80.))
            cosH0 = -sinDec * SLAt / (cLat * Sqrt(1. - (sinDec) ** 2))

            If (cosH0 > 1.) Then
                fDayLength(ii) = 0.
            ElseIf (cosH0 < -1.) Then
                fDayLength(ii) = 1.
            Else
                fDayLength(ii) = Acos(cosH0) / Pi
            End If

!           calculate seconds of the day when the sun is up
            DayLength(ii) = fDayLength(ii) * 86400. 

!           calculate temperature modifier
            if (Tav(ii) <= Tmin) then 
                fT = 0
            else if (Tav(ii) >= Tmax) then
                fT = 0
            else 
                fT = ((Tav(ii) - Tmin) / (Topt - Tmin)) * ((Tmax - Tav(ii)) / (Tmax - Topt))**((Tmax - Topt) / (Topt - Tmin))
            end if

!           calculate VPD modifier
            fVPD = Exp(-CoeffCond * VPD(ii))

!           calculate soil water modifier
            MoistRatio = ASW / MaxASW
            fSW = 1. / (1. + ((1. - MoistRatio) / SWconst) ** SWpower)

!           calculate soil nutrition modifier
            if (fNn == 0) then
                fNutr = 1.
            else
                fNutr = 1. - (1. - fN0) * (1. - FR) ** fNn
            end if

!           calculate frost modifier
            fFrost = 1. - kF * (FrostDays (ii) /30.)

!           calculate age modifier
            if (nAge == 0) then
                fAge = 1
            else
                RelAge = StandAge / MaxAge
                fAge = (1. / (1. + (RelAge / rAge) ** nAge))
            end if

!           calculate CO2 modifiers
            fCalpha = fCalphax * CO2 / (350 * (fCalphax - 1) + CO2)
            fCg = fCg0 / (1 + (fCg0 - 1) * CO2 / 350)

!           calculate physiological modifier applied to conductance and APARu
            PhysMod = min(fVPD, fSW) * fAge


!           determine gross and net biomass production
!           canopy cover and light interception.
            If (fullCanAge > 0. .and. StandAge < fullCanAge) then
                CanCover = (StandAge + 0.01) / fullCanAge
            Else
                CanCover = 1.
            end if
            lightIntcptn = (1 - (Exp(-k * LAI / CanCover)))

!           calculate PAR, APAR, APARu, GPP and NPP
            alphaC = alpha * fNutr * fT * fFrost * fCalpha * physMod
            epsilon = gDM_mol * molPAR_MJ * alphaC
            RAD = SolarRad(ii) * daysInMonth(month)        !    calculate CO2 modifiers
            RADint = RAD * lightIntcptn * CanCover
            GPP = epsilon * RADint / 100               !tDM/ha
            NPP = GPP * rg                             !assumes respiratory rate is constant
            !NPP = GPPdm - (GPPdm * rg) - (((WF * rf) + ((WS + WR) * rw)) * (Q10 ** ((Tav(ii) - 20) / 10)))   !tDM/ha  
            !if (NPP < 0) then
            !    NPP = 0 
            !end if


!           Now do the water balance ...
!           Penman-Monteith equation for computing canopy transpiration
!           in kg/m2/day, which is conmverted to mm/day.

            if (LAI <= LAIgcx) then
                gC = MinCond + (MaxCond - MinCond) * LAI / LAIgcx
            else
                gC = MaxCond
            end if

            CanCond = gC * PhysMod * fCg
            if (CanCond == 0) Then 
                CanCond = 0.0001
            end if

            netRad = Qa + qb * (SolarRad(ii) * 10 ** 6 / DayLength(ii))                ! SolarRad in MJ/m2/day --> W/m2
            defTerm = rhoAir * lambda * (VPDconv * VPD(ii)) * BLcond
            div = CanCond * (1 + e20) + BLcond
            Etransp = CanCond * (e20 * netRad + defTerm) / div           ! in J/m2/s
            CanopyTranspiration = Etransp / lambda * DayLength(ii)         ! converted to kg/m2/day

!           transpiration from Penman-Monteith (mm/day converted to mm/month)
            Transp = daysInMonth(month) * CanopyTranspiration

!           rainfall interception
            if (LAImaxIntcptn > 0.) then 
                fRainInt = MaxIntcptn * Min(1., LAI / LAImaxIntcptn)
            else
                fRainInt = MaxIntcptn 
            end if
            RainInt = Rain(ii) * fRainInt

!           do soil water balance
            SupIrrig = 0.
            RunOff = 0.
            ASW = ASW + Rain(ii) + (100. * Irrig / 12.) + pooledSW
            EvapTransp = Min(ASW, Transp + RainInt)          !ET can not exceed ASW
            excessSW = max(ASW - EvapTransp - MaxASW, 0.)
            ASW = ASW - EvapTransp - excessSW
            pooledSW = poolFractn * excessSW
            RunOff = (1. - poolFractn) * excessSW

            if (ASW < MinASW) then
                SupIrrig = MinASW - ASW
                ASW = MinASW
            end if

!           correct for actual ET
            TranspScaleFactor = EvapTransp / (Transp + RainInt)   !scales NPP and GPP
            GPP = TranspScaleFactor * GPP
            NPP = TranspScaleFactor * NPP
            WUE = NPP / EvapTransp

!           determine biomass increments and losses
!           calculate partitioning coefficients
            m = m0 + (1 - m0) * FR
            pFS = pfsConst * avDBH ** pfsPower
            pR = pRx * pRn / (pRn + (pRx - pRn) * PhysMod * m)
            pS = (1 - pR) / (1 + pFS)
            pF = 1 - pR - pS

!           calculate biomass increments
            incrWF = NPP * pF
            incrWR = NPP * pR
            incrWS = NPP * pS

!           calculate litterfall & root turnover -
            lossWF = gammaF * WF
            lossWR = gammaR * WR

!           Calculate end-of-month biomass
            WF = WF + incrWF - lossWF
            WR = WR + incrWR - lossWR
            WS = WS + incrWS
            WL = WL + lossWF
            TotalW = WF + WR + WS


!           Calculate soil carbon balance
!           First calculate the decomposition rate...
            kr = krmax * fSW * fT
            kl = klmax * fSW * fT
            ko = komax * fSW * fT

!           and then calculate the fluxes in, out and between carbon and nitrogen pools
            Yl_Coutflux = kl * (1 - hc) * Yl_C
            Yr_Coutflux = kr * (1 - hc) * Yr_C
            humification_l = kl * hc * Yl_C
            humification_r = kr * hc * Yr_C
            O_Coutflux = ko * O_C
            Yl_Noutflux = kl * ((1 - hc) / (1 - el)) * (Yl_N - el * (Yl_C / qbc))
            humification_N_l = kl * hc * (Yl_N / qh)
            humification_N_r = kr * hc * (Yr_N / qh)
            Yr_Noutflux = kr * ((1 - hc) / (1 - er)) * (Yr_N - er * (Yr_C / qbc))
            O_Noutflux = ko * O_N

!           Now calculate the end-of-month carbon and nitrogen pools
            Yr_C = Yr_C !+ (StemMort / 2) - Yr_Coutflux - humification_r
            Yl_C = Yl_C !+ ((lossWF + lossWR + FoliageMort + RootMort) / 2) - Yl_Coutflux - humification_l
            O_C = O_C + humification_l + humification_r - O_Coutflux
            Yr_N = Yr_N + (StemMort / (2 * qir)) - Yr_Noutflux - humification_N_r
            Yl_N = Yl_N + ((lossWF + lossWR + FoliageMort + RootMort) / (2 * qil)) - Yl_Noutflux - humification_N_l
            O_N = O_N + humification_N_r + humification_N_l - O_Noutflux

            TotalCarbo = Yr_C + Yl_C + O_C
            TotalNitro = Yr_N + Yl_N + O_N

  !Calculate the Fertility Rating
    
        !First calculate available nitrogen and uptake
        !Nav = Yr_Noutflux + Yl_Noutflux + O_Noutflux
        !Un = delWF * Ncf / 2
        
        !Now estimate FR
        
        !If Un = 0 Then FR = 1 Else FRin = Nav / Un
        
        !If FRin > 1 Then FR = 1 Else If FRin < 0 Then FR = 0 Else FR = FRin
        
        !FRsum = FRsum + FR

            GPP_C = GPPdm * dmC
            NPP_C = NPP * dmC
            Raut = GPP_C - NPP_C
            Rhet = Yl_Coutflux + Yr_Coutflux + O_Coutflux
            Reco = Raut + Rhet
            NEP = GPP_C - Reco

!           Update tree and stand data at the end of this time period,
!           taking mortality, thinning or defoliation into account

            StandAge = StandAge + 1./12.

!           Perform thinning or defoliation events for this time period
!           need to add thinning and defoliation rootins
            if (nThinning > 0 .and. countThinning <= nThinning) then 
                if (StandAge >= site_thinning(countThinning,1)) then
                    if (StemNo > site_thinning(countThinning,2)) then
                        delN = (StemNo - site_thinning(countThinning,2)) / StemNo
                        StemNo = StemNo * (1 - delN)
                        WF = WF * (1 - delN * site_thinning(countThinning,3))
                        WR = WR * (1 - delN * site_thinning(countThinning,4))
                        WS = WS * (1 - delN * site_thinning(countThinning,5))
                    end if
                    countThinning = countThinning + 1
                    if (countThinning > nThinning) then
                        deallocate(site_thinning)
                    end if
                end If
            end If

!           calculate age and stress-related mortality
            gammaN = gammaN1 + (gammaN0 - gammaN1) * Exp(-ln2 * (StandAge / tgammaN) ** ngammaN)

            if (gammaN > 0) then
                delStems = gammaN * StemNo / 12 /100
                FoliageMort = mF * delStems * (WF / StemNo)
                RootMort = mR * delStems * (WR / StemNo)
                StemMort = mS * delStems * (WS / StemNo)
                WF = WF - mF * delStems * (WF / StemNo)
                WR = WR - mR * delStems * (WR / StemNo)
                WS = WS - mS * delStems * (WS / StemNo)
                StemNo = StemNo - delStems
                mortality = mortality + delStems
            end if

!           calculate self-thinning mortality
            wSmax = wSx1000 * (1000 / StemNo) ** thinPower
            AvStemMass = WS * 1000 / StemNo
            delStems = 0 

            if (wSmax < AvStemMass) then
                accuracy = 1 / 1000
                n = StemNo / 1000
                x1 = 1000 * mS * WS / StemNo
                j = 0
                do
                    j = j + 1
                    x2 = wSx1000 * n ** (1 - thinPower)
                    fN = x2 - x1 * n - (1 - mS) * WS
                    dfN = (1 - thinPower) * x2 / n - x1
                    dN = -fN / dfN
                    n = n + dN
                    if (abs(dN) <= accuracy) exit 
                        if (j >= 5) exit
                end do
                getMortality = StemNo - 1000 * n
                delStems = getMortality
                FoliageMort = FoliageMort + mF * delStems * (WF / StemNo)
                RootMort = RootMort + mR * delStems * (WR / StemNo)
                StemMort = StemMort + mS * delStems * (WS / StemNo)
                WF = WF - mF * delStems * (WF / StemNo)
                WR = WR - mR * delStems * (WR / StemNo)
                WS = WS - mS * delStems * (WS / StemNo)
                StemNo = StemNo - delStems
                wSmax = wSx1000 * (1000 / StemNo) ** thinPower
                AvStemMass = WS * 1000 / StemNo
                selfThin = selfThin + delStems
            end if

!           update age-dependent factors
            SLA = SLA1 + (SLA0 - SLA1) * Exp(-ln2 * (StandAge / tSLA) ** 2.)
            fracBB = fracBB1 + (fracBB0 - fracBB1) * Exp(-ln2 * (StandAge / tBB))
            Density = rho1 + (rho0 - rho1) * Exp(-ln2 * (StandAge / tRho))
            if (tgammaF * gammaF1 == 0) then
                gammaFoliage = gammaF1
            else
                kgammaF = 12 * Log(1 + gammaF1 / gammaF0) / tgammaF
                gammaFoliage = gammaF1 * gammaF0 / (gammaF0 + (gammaF1 - gammaF0) * Exp(-kgammaF * StandAge))
            end if
            gammaF = gammaFoliage
  

!           update stand characteristics
            LAI = WF * SLA * 0.1
            avDBH = StemConst*(((WS+WF) * 1000. )/ StemNo) ** (StemPower) !(AvStemMass/StemConst) ** (1/StemPower) !
            BasArea = (((avDBH / 200.) ** 2) * Pi) * StemNo
            Height = aH*((WS + WF) * 1000. / StemNo)**bW! * StemNo ** cD

            if (aV > 0) then
                StandVol = aV * avDBH ** nVB * StemNo ** nVN 
            else
                StandVol = WS * (1 - fracBB) / Density  
            end if

            CVI = StandVol - oldVol
            oldVol = StandVol  
            if (StandAge > 0) then
                MAI = StandVol / StandAge
            else
                MAI = 0
            end if

!           calculate annual NEP
            do ijj=1,11
                NEPmat(ijj)=NEPmat(ijj+1)
            end do

            NEPmat(12)=NEP
            aNEP=sum(NEPmat)


! ----------------------------------------------------------------------------------------
!   Write the output


            outs( 1 ) = StandAge
            outs( 2 ) = StemNo
            outs( 3 ) = BasArea
            outs( 4 ) = StandVol
            outs( 5 ) = avDBH
            outs( 6 ) = MAI
            outs( 7 ) = SLA
            outs( 8 ) = CanCover
            outs( 9 ) = LAI
            outs( 10 ) = WF
            outs( 11 ) = WR
            outs( 12 ) = WS
            outs( 13 ) = WL
            outs( 14 ) = TotalW
            outs( 15 ) = AvStemMass
            outs( 16 ) = fracBB
            outs( 17 ) = fAge
            outs( 18 ) = fVPD
            outs( 19 ) = fT
            outs( 20 ) = fCalpha
            outs( 21 ) = fCg
            outs( 22 ) = fFrost
            outs( 23 ) = fSW
            outs( 24 ) = fNutr
            outs( 25 ) = PhysMod
            outs( 26 ) = GPP
            outs( 27 ) = NPP
            outs( 28 ) = RadInt
            outs( 29 ) = alphaC
            outs( 30 ) = epsilon
            outs( 31 ) = CVI
            outs( 32 ) = m
            outs( 33 ) = pR
            outs( 34 ) = pS
            outs( 35 ) = pF
            outs( 36 ) = pFS
            outs( 37 ) = gammaF
            outs( 38 ) = lossWF
            outs( 39 ) = lossWR
            outs( 40 ) = wSmax
            outs( 41 ) = gammaN
            outs( 42 ) = Mortality
            outs( 43 ) = supIrrig
            outs( 44 ) = RunOff
            outs( 45 ) = fRainInt
            outs( 46 ) = RainInt
            outs( 47 ) = CanCond
            outs( 48 ) = WUE
            outs( 49 ) = EvapTransp
            outs( 50 ) = Transp
            outs( 51 ) = ASW
            outs( 52 ) = NEP
            outs( 53 ) = Rhet
            outs( 54 ) = Yr_C
            outs( 55 ) = Yl_C
            outs( 56 ) = O_C
            outs( 57 ) = Yr_N
            outs( 58 ) = Yl_N
            outs( 59 ) = O_N

!           store output
            y(ii,1:nvariables,siteNo) = outs(vars)
            !open (20, file='output.txt', action='write')
            !write (20,21) y(ii,1), y(ii,2), y(ii,3)
            !21 format (7f20.10)

        end do

    end do

end subroutine model
